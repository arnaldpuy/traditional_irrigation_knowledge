coding_schema_fun <- function(prompt_choice = c("analytical", "subcategory")) {

  prompt_choice <- match.arg(prompt_choice)

  nullable_str <- list(type = c("string", "null"))

  if (prompt_choice == "analytical") {

    props <- list(
      id_cite            = list(type = "string"),
      epistemic_type     = list(type = "string",
        enum = c("indicator", "heuristic_rule", "mechanistic_belief",
                 "calendrical", "astronomical", "normative_social",
                 "valuation")),
      functions          = list(type = "array",
        items = list(type = "string",
          enum = c("crop_production", "water_conservation", "risk_management",
                   "biodiversity", "soil_health", "microclimate_regulation",
                   "pest_control", "social_cohesion", "medicinal_food",
                   "recreation_wellbeing", "livestock", "groundwater_recharge"))),
      efficiency_legible = list(type = "boolean"),
      sensory_channels   = list(type = "array",
        items = list(type = "string",
          enum = c("visual", "tactile", "acoustic", "olfactory", "gustatory",
                   "none"))),
      n_env_variables    = list(type = "integer"),
      eroded_by          = list(type = "array",
        items = list(type = "string",
          enum = c("drip", "soil_sensor", "remote_sensing", "on_demand",
                   "app_scheduling", "none"))),
      corroboration      = list(anyOf = list(
        list(type = "string",
             enum = c("mechanistic", "plausible", "coordinative", "dubious")),
        list(type = "null"))),
      measured_variable  = list(type = "array",
        items = list(type = "string")),
      time_horizon       = list(type = "string",
        enum = c("immediate", "days", "seasonal", "annual", "generational")),
      inferential_form   = list(type = "string",
        enum = c("deterministic", "conditional", "hedged")),
      temporal_framing   = list(type = "string",
        enum = c("present", "past_elder", "saying")),
      knowledge_mode     = list(type = "string",
        enum = c("know_how", "know_that")),
      is_saying          = list(type = "boolean"),
      is_value_statement = list(type = "boolean"),
      cue                = nullable_str,
      inference          = nullable_str,
      action             = nullable_str,
      practice           = list(type = "string"),
      confidence         = list(type = "number"),
      rationale          = list(type = "string"))

    schema <- list(type = "object", properties = props,
                   required = names(props), additionalProperties = FALSE)

  } else {

    crop <- c("Irrigation water needs based on crop type",
              "Growth stage of the plant",
              "Visual indicator of water stress",
              "Adaptation to water availability",
              "Final crop quality",
              "Crop association, pest management and biodiversity")
    irrig <- c("Optimum time for irrigation", "Irrigation turn",
               "Water use rights",
               "Organization of the irrigators' community",
               "Traditional irrigation technique",
               "Management of the infrastructure", "Irrigation indicator",
               "Cultural value of tradition")
    soil <- c("Visual and tactile indicator of moisture", "Type of soil",
              "Irrigation frequency", "Practices to preserve moisture",
              "Roots system", "Surface runoff management",
              "Drainage management", "Soil nutrient management")
    weather <- c("Interpreting atmospheric phenomena", "Agricultural calendar",
                 "Temperature relationship to irrigation scheduling",
                 "Biological indicator of weather")
    all_subcats <- c(crop, irrig, soil, weather)
    main_cats <- c("Crop knowledge and management",
                   "Irrigation practices and communal water management",
                   "Soil and moisture management",
                   "Weather and climate conditions")

    cross <- list(type = c("object", "null"),
      properties = list(
        subcategory   = list(type = "string", enum = all_subcats),
        main_category = list(type = "string", enum = main_cats)),
      required = c("subcategory", "main_category"),
      additionalProperties = FALSE)

    props <- list(
      id_cite               = list(type = "string"),
      primary_subcategory   = list(type = "string", enum = all_subcats),
      secondary_subcategory = list(anyOf = list(
        list(type = "string", enum = all_subcats),
        list(type = "null"))),
      cross_category        = cross,
      confidence            = list(type = "number"),
      rationale             = list(type = "string"),
      main_category_flag    = list(anyOf = list(
        list(type = "string", enum = main_cats),
        list(type = "null"))))

    schema <- list(type = "object", properties = props,
                   required = names(props), additionalProperties = FALSE)
  }

  schema
}
