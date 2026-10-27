build_coding_request_fun <- function(custom_id, system_prompt, user_text,
                                     schema, model = "claude-opus-4-8",
                                     max_tokens = 600L, effort = "high",
                                     thinking = "disabled", cache_ttl = "1h") {

  list(
    custom_id = custom_id,
    params = list(
      model = model,
      max_tokens = max_tokens,
      thinking = list(type = thinking),
      output_config = list(
        effort = effort,
        format = list(type = "json_schema", schema = schema)),
      system = list(list(type = "text", text = system_prompt,
                         cache_control = if (identical(cache_ttl, "5m")) {
                           list(type = "ephemeral")
                         } else {
                           list(type = "ephemeral", ttl = cache_ttl)
                         })),
      messages = list(list(role = "user", content = user_text))))
}
