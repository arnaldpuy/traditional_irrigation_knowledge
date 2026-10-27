run_three_models_fun <- function(items, system_prompt, schema, tag, out_dir,
                                 anthropic_model = "claude-opus-4-8",
                                 deepseek_model = "deepseek-chat",
                                 openai_model = "gpt-5.4",
                                 max_tokens = 400L,
                                 anthropic_poll = 30, openai_poll = 20L,
                                 deepseek_temperature = 0.7,
                                 deepseek_concurrency = 8L) {

  run_provider <- function(prov, runner) {
    cache <- file.path(out_dir, sprintf("%s_raw_%s.csv", tag, prov))
    if (file.exists(cache)) {
      message(sprintf("  [%s] cache hit -> %s", prov, basename(cache)))
      res <- data.table::as.data.table(
        utils::read.csv(cache, stringsAsFactors = FALSE, encoding = "UTF-8"))
    } else {
      res <- runner()
      if (any(res$status == "succeeded")) {
        data.table::fwrite(res, cache)
      } else {
        warning(sprintf("[%s/%s] no request succeeded; result NOT cached", tag, prov))
      }
    }
    res[, provider := prov][]
  }

  res_a <- run_provider("anthropic", function() {
    requests <- lapply(seq_len(nrow(items)), function(i)
      build_coding_request_fun(items$custom_id[i], system_prompt,
                               items$user_text[i], schema, model = anthropic_model,
                               max_tokens = max_tokens, thinking = "disabled"))
    b <- submit_batch_fun(requests)
    b <- poll_batch_fun(b$id, interval = anthropic_poll)
    fetch_batch_results_fun(b)
  })

  res_d <- run_provider("deepseek", function()
    run_deepseek_batch_fun(items, system_prompt, model = deepseek_model,
                           temperature = deepseek_temperature,
                           max_tokens = max_tokens,
                           concurrency = deepseek_concurrency))

  res_o <- run_provider("openai", function()
    run_openai_batch_fun(items, system_prompt, model = openai_model,
                         reasoning_effort = "low",
                         max_completion_tokens = max_tokens,
                         poll_seconds = openai_poll))

  data.table::rbindlist(list(res_a, res_d, res_o), use.names = TRUE, fill = TRUE)
}
