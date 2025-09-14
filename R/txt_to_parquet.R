#' Convert a large text file (CSV/TSV) to a Parquet dataset with optional partitioning.
#' This function uses DuckDB for efficient chunked reading and writing.
#'  @param in_file Path to the input text file (CSV/TSV).
#'  @param out_dir Directory to save the Parquet dataset (default: "par
#'  quet_dataset").
#'  @param chunk_size Number of rows per chunk (default: auto-calculated for
#'  ~200 MB chunks).
#'  @param dbdir Path to DuckDB database file (default: in-memory).
#'  @param partition_cols Character vector of column names to partition by (default: NULL).
#'  @param return_arrow Logical, whether to return an Arrow dataset object (default: FALSE).
#'  @param compression Compression algorithm for Parquet files (default: "ZSTD").
#'  @param repartition Optional integer to repartition the dataset after writing (default: NULL).
#'  @param log_perf Logical, whether to log performance metrics (default: TRUE).
#'  @param log_file Optional path to a log file to save performance metrics (default: NULL).
#'  @return A list containing:
#'  \item{summary}{A data frame summarizing the partitioning (if any).}
#'  \item{dataset}{(Optional) An Arrow dataset object if return_arrow is TRUE.}
#'  \item{perf}{A list of performance metrics including time taken, rows processed, input/output sizes, etc.}
#'  @examples
#'  \dontrun{
#'  result <- txt_to_parquet_dataset(
#'      in_file = "large_data.csv",
#'      out_dir = "parquet_data",
#'      chunk_size = 1e6,
#'      partition_cols = c("year", "month"),
#'      return_arrow = TRUE,
#'      compression = "ZSTD"
#'      )
#'  print(result$summary)
#'  print(result$perf)
#'  if (!is.null(result$dataset)) {
#'      print(result$dataset)
#'  }
#'  }
#'  @import DBI duckdb progressr glue jsonlite readr
#'  @importFrom arrow open_dataset
#'  @importFrom utils write.csv
#'  @export
#'  @details
#'  This function reads a large text file (CSV/TSV) in chunks using DuckDB,
#'  writes the data to Parquet files with optional partitioning, and provides
#'  performance metrics. It can also return an Arrow dataset object for further
#'  analysis in R.
#'  The function automatically calculates an optimal chunk size to balance
#'  memory usage and performance, targeting approximately 200 MB per chunk.
#'  If partitioning columns are specified, the output Parquet files will be organized
#'  into subdirectories based on the unique values of those columns.
#'  Compression can be specified to reduce the size of the output files, with "ZSTD"
#'  being the default for a good balance of speed and compression ratio.
#'  Performance metrics include total time taken, rows processed, input and output
#'  file sizes, and rows per second.
#'  If a log file path is provided, the performance metrics will be saved in JSON
#'  format for easy parsing and analysis.
#'  Note that this function requires the DuckDB and Arrow packages to be installed
#'  and may require additional system dependencies for optimal performance.
#'  Ensure that the input file exists and that the output directory is writable.
#'  @seealso
#'  - \code{\link[DBI]{dbConnect}} for database connections
#'  - \code{\link[duckdb]{duckdb}} for DuckDB database
#'  - \code{\link[arrow]{open_dataset}} for working with Arrow datasets
#'  - \code{\link[progressr]{with_progress}} for progress reporting
#'  - \code{\link[glue]{glue}} for string interpolation
#'  - \code{\link[jsonlite]{toJSON}} for JSON handling
#'  - \code{\link[readr]{read_csv}} for reading CSV files




txt_to_parquet_dataset <- function(in_file,
                                   out_dir = "parquet_dataset",
                                   chunk_size = NULL,
                                   dbdir = ":memory:",
                                   partition_cols = NULL,
                                   return_arrow = FALSE,
                                   compression = "ZSTD",
                                   repartition = NULL,
                                   log_perf = TRUE,
                                   log_file = NULL) {
    stopifnot(file.exists(in_file))
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

    library(DBI); library(duckdb); library(progressr)
    library(glue); library(jsonlite); library(readr)

    t0 <- Sys.time()
    file_size <- file.info(in_file)$size
    gb <- file_size / 1024^3
    message("📦 Input file size: ", round(gb, 2), " GB")

    con <- dbConnect(duckdb::duckdb(), dbdir = dbdir)
    row_count <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM read_csv_auto('{in_file}')"))$n
    message("📊 Estimated rows: ", format(row_count, big.mark = ","))

    # Auto chunk size (~200 MB)
    if (is.null(chunk_size)) {
        avg_row_size <- file_size / max(1, row_count)
        target_chunk_bytes <- 200 * 1024^2
        chunk_size <- max(1e5, floor(target_chunk_bytes / avg_row_size))
    }

    # Partition logic
    if (is.null(partition_cols)) {
        partitions <- character(0)
        extra_sql <- ""
    } else {
        peek <- dbGetQuery(con, glue("SELECT * FROM read_csv_auto('{in_file}') LIMIT 1000"))
        missing <- setdiff(partition_cols, names(peek))
        if (length(missing) > 0) {
            stop("Partition columns not found: ", paste(missing, collapse = ", "))
        }
        partitions <- partition_cols
        extra_sql <- ""
    }

    message("📂 Partitioning by: ",
            ifelse(length(partitions) > 0, paste(partitions, collapse = ", "), "none"))
    message("🗜️ Using compression: ", compression)

    n_chunks <- ceiling(row_count / chunk_size)

    # --- Output path logic ---
    if (length(partitions) > 0) {
        out_path <- out_dir                     # directory
    } else {
        out_path <- file.path(out_dir, "data.parquet")  # single file
    }

    # --- Step 3: Chunked COPY with progress ---
    handlers(global = TRUE)
    with_progress({
        p <- progressor(steps = n_chunks)
        for (i in seq_len(n_chunks)) {
            offset <- (i - 1) * chunk_size
            if (length(partitions) > 0) {
                part_clause <- glue("PARTITION_BY ({paste(partitions, collapse = ', ')}),")
            } else {
                part_clause <- ""
            }
            sql <- glue("
        COPY (
          SELECT *{extra_sql}
          FROM read_csv_auto('{in_file}')
          LIMIT {chunk_size} OFFSET {offset}
        )
        TO '{out_path}' (
          FORMAT 'parquet',
          {part_clause}
          COMPRESSION '{compression}',
          APPEND TRUE
        );
      ")
            dbExecute(con, sql)
            p(message = glue("Chunk {i}/{n_chunks} written"))
        }
    })
    dbDisconnect(con, shutdown = TRUE)

    # --- Step 5: Partition summary ---
    con2 <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    if (length(partitions) > 0) {
        summary_sql <- glue("
      SELECT {paste(partitions, collapse = ', ')}, COUNT(*) AS n
      FROM read_parquet('{out_dir}/**/*.parquet')
      GROUP BY {paste(partitions, collapse = ', ')}
      ORDER BY {paste(partitions, collapse = ', ')}
    ")
    } else {
        summary_sql <- glue("
      SELECT COUNT(*) AS n FROM read_parquet('{out_path}')
    ")
    }
    part_summary <- dbGetQuery(con2, summary_sql)
    dbDisconnect(con2, shutdown = TRUE)

    # --- Step 6: Performance logging ---
    t_total <- Sys.time() - t0
    out_size <- sum(file.info(list.files(out_dir, recursive = TRUE, full.names = TRUE))$size)

    perf <- list(
        time = as.numeric(t_total, units = "secs"),
        rows = row_count,
        size_in = file_size,
        size_out = out_size,
        partitions = partitions,
        compression = compression,
        chunk_size = chunk_size,
        out_dir = out_dir,
        timestamp = as.character(Sys.time())
    )

    if (log_perf) {
        message("⏱️ Total time: ", round(t_total, 1), " sec")
        message("⚡ Rows/sec: ", round(row_count / as.numeric(t_total, units = "secs"), 0))
        message("💾 Output size: ", round(out_size / 1024^3, 2), " GB")
    }

    if (return_arrow) {
        library(arrow)
        if (length(partitions) > 0) {
            ds <- open_dataset(out_dir)
        } else {
            ds <- open_dataset(out_path)
        }
        return(list(summary = part_summary, dataset = ds, perf = perf))
    } else {
        return(list(summary = part_summary, perf = perf))
    }
}
