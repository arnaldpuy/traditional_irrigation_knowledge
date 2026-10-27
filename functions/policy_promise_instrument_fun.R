# The promise-map instrument of code/18 (prompt and JSON schema), made callable

policy_promise_instrument_fun <- function(scope_note = NULL) {
  fun_vocab <- c("crop_production", "water_conservation", "soil_health",
                 "risk_management", "social_cohesion", "biodiversity",
                 "microclimate_regulation", "pest_control", "groundwater_recharge",
                 "livestock", "medicinal_food", "recreation_wellbeing")
  tech_vocab <- c("drip", "soil_sensor", "remote_sensing", "on_demand",
                  "app_scheduling", "flow_metering", "modernization_general",
                  "other")

  policy_prompt <- '
  SYSTEM ROLE
  You extract the PROMISE SPACE of irrigation-modernisation policy: the functions
  that a policy or technical document attributes to modernisation and its
  technologies, using a fixed function vocabulary.
  
  TASK
  Given the text of a policy plan or technical manual, report:
  (a) which modernisation technologies it names or promotes, and
  (b) for each of a FIXED set of agroecosystem functions the document invokes,
      whether it PROMISES the technology delivers that function (direction
      "delivered") or ADMITS a trade-off / harm to it (direction "degraded"),
      with the scale, the consistency of the claim, and its prominence.
  
  FUNCTION VOCABULARY (use these exact keys; never invent one)
  - crop_production: crop yield, growth, quality, agricultural production/output
  - water_conservation: water saving, efficiency, reduced abstraction, water productivity
  - soil_health: soil structure, fertility, salinity/erosion control
  - risk_management: drought/shortage resilience, supply guarantee, reliability, stability
  - social_cohesion: collective governance, equity of distribution, rural viability, cooperation
  - biodiversity: habitat, ecosystems, species, environmental/nature protection
  - microclimate_regulation: local climate, temperature/humidity, frost protection
  - pest_control: pest, weed, disease suppression
  - groundwater_recharge: aquifer recovery, return flows, water-table protection
  - livestock: forage, animal husbandry integration
  - medicinal_food: wild food and medicinal plants
  - recreation_wellbeing: landscape, cultural, amenity, recreational, wellbeing value
  
  RULES
  - Code a function ONLY if the document invokes it in connection with modernisation.
  - direction: "delivered" (promised benefit) | "degraded" (admitted trade-off) | "mixed".
  - scale: "field" | "farm" | "basin" | "regional" | "mixed" | "unclear".
  - consistency: "consistent" | "mixed" | "contested".
  - evidence_strength = PROMINENCE: "strong" (headline objective/justification),
    "moderate" (stated benefit), "weak" (passing mention).
  - justification: <= 25 words grounded in the text.
  - off_vocabulary_benefits: promised benefits outside the 12 functions (energy,
    competitiveness, jobs, modernisation/digitalisation as an end, food security), or "none".
  - technologies from: drip, soil_sensor, remote_sensing, on_demand, app_scheduling,
    flow_metering, modernization_general (use when the promise is about modernisation
    as a whole, not a named technology), other.
  - evidence_tier: "national_plan" | "regional_plan" | "eu_regulation" | "technical_manual".
  - dominant_scale: the spatial scale the document mostly addresses.
  
  OUTPUT
  Return ONLY this JSON object:
  {"technologies": ["<one or more tech keys>"],
   "evidence_tier": "<tier>",
   "dominant_scale": "<scale>",
   "function_findings": [
     {"function": "<vocab key>", "direction": "delivered|degraded|mixed",
      "scale": "<scale>", "consistency": "<consistency>",
      "evidence_strength": "strong|moderate|weak", "justification": "<=25 words>"}],
   "off_vocabulary_benefits": "<short text or none>",
   "notes": "<=20 words"}'
  
  finding_schema <- list(
    type = "object",
    properties = list(
      `function` = list(type = "string", enum = as.list(fun_vocab)),
      direction = list(type = "string", enum = list("delivered", "degraded", "mixed")),
      scale = list(type = "string",
                   enum = list("field", "farm", "basin", "regional", "mixed", "unclear")),
      consistency = list(type = "string",
                         enum = list("consistent", "mixed", "contested")),
      evidence_strength = list(type = "string", enum = list("strong", "moderate", "weak")),
      justification = list(type = "string")),
    required = list("function", "direction", "scale", "consistency",
                    "evidence_strength", "justification"),
    additionalProperties = FALSE)
  
  policy_schema <- list(
    type = "object",
    properties = list(
      technologies = list(type = "array",
                          items = list(type = "string", enum = as.list(tech_vocab))),
      evidence_tier = list(type = "string",
                           enum = list("national_plan", "regional_plan",
                                       "eu_regulation", "technical_manual")),
      dominant_scale = list(type = "string",
                            enum = list("field", "farm", "basin", "regional",
                                        "mixed", "unclear")),
      function_findings = list(type = "array", items = finding_schema),
      off_vocabulary_benefits = list(type = "string"),
      notes = list(type = "string")),
    required = list("technologies", "evidence_tier", "dominant_scale",
                    "function_findings", "off_vocabulary_benefits", "notes"),
    additionalProperties = FALSE)
  
  if (!is.null(scope_note)) policy_prompt <- paste0(policy_prompt, "\n\n", scope_note)
  list(prompt = policy_prompt, schema = policy_schema,
       fun_vocab = fun_vocab, tech_vocab = tech_vocab)
}
