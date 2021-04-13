cogVars <- c(
  'supernatural',
  'narrow_specialist',
  'broad_generalist',
  'divination',
  'herbal',
  'zoology',
  'psychology',
  'injury'
)

socialVars_specialist <- c(
  'hierarchy_within_domain',
  'distributed_expertise',
  'experts_collaborate',
  'experts_compete',
  'rare_knowledge',
  'common_knowledge',
  'secretive_knowledge',
  'prestige',
  'low_status',
  'selfish',
  'generosity',
  'political_influence',
  'domain_general_influence',
  'religious_leader',
  'public_performance',
  'private_performance',
  'costly_initiation',
  'costly_lifestyle',
  'costly_ritual',
  'ornamentation',
  'charismatic',
  'anti_hierarchy',
  'learned_kin',
  'learned_purchased',
  'learned_revelation',
  'seek_proximity',
  'deference'
)

socialVars_layperson <- c(
  'confers_benefits',
  'imposes_costs',
  'uncommon_serious',
  'common_problem',
  'evidence_success',
  'evidence_failure',
  'rationalize_failures',
  'reputation_efficacy',
  'patronage_based_efficacy',
  'receives_payment',
  'trust',
  'distrust',
  'avoided',
  'no_payment',
  'anti_reputation_efficacy',
  'patron_socioeconomic',
  'advice',
  'social_learning',
  'lives_faraway'
)

incl_vars <- c(cogVars, socialVars_layperson, socialVars_specialist)
vars_general_chars <- c(
  'male',
  'female',
  'child_adolescent',
  'adult',
  'older_elderly'
)

vars_expert_chars <- c(
  'political_influence',
  'religious_leader',
  'supernatural',
  'divination'
)

rel_vars <- c(
  "costly_ritual",
  "prescribe_ritual_sacrifice",
  "prescribe_taboos_prohibitions",
  "learned_revelation"
)

vars_rec_positive <- c(
  'lives_nearby',
  'uncommon_serious',
  'common_problem',
  'evidence_success',
  'used_after_other_failed',
  'no_payment',
  'confers_benefits',
  'trust',
  'social_learning',
  'rationalize_failures',
  'reputation_efficacy',
  'patronage_based_efficacy',
  'prestige',
  'patron_socioeconomic',
  'advice',
  'rare_knowledge'    
)

vars_rec_negative <- c(
  'lives_faraway',
  'imposes_costs',
  'receives_payment',
  'distrust',
  'avoided',
  'anti_reputation_efficacy',
  'evidence_failure',
  'low_status'
)
