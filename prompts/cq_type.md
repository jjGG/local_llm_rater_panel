# Stage 2 prompt: assign the cultural-intelligence subclassification.
#
# Called only for passages whose stage-1 codes include one of the codes listed
# under `subclassification.applies_to` in the codebook (v4: "Positive").
#
# At `level: item` (v4) the model returns one of the 20 questionnaire-item codes
# of the Cultural Intelligence Scale and the FACTOR IS DERIVED from it in R, so
# the two levels of the scheme cannot contradict each other. There is
# deliberately no "None" option: the response schema forces a choice, mirroring
# the human protocol, which allowed none either.
#
# Placeholders:
#   {{factors_block}}    the four factors, each with its questionnaire items
#   {{choice_list}}      the codes the model may return, comma separated
#   {{answer_block}}     how to answer, per subclassification level
#   {{rules_block}}      decision rules from the codebook
#   {{trigger_code}}     the stage-1 code that triggered this call
#   {{assigned_codes}}   all stage-1 codes assigned to this passage
#   {{context_block}}    empty in context_mode=unit
#   {{unit_text}}        the passage to code

### SYSTEM
You are assisting with a qualitative content analysis of student reflection
journals from an international online collaborative course.

A trained coder has already coded the passage below as "{{trigger_code}}". Your
task is to say which capability of cultural intelligence (Ang & Van Dyne) the
passage expresses. The scheme has four factors, each measured by the
questionnaire items listed under it. Treat the item wording as the reference
meaning.

{{factors_block}}

Decision rules:
{{rules_block}}

{{answer_block}}

How to work:
- Judge the passage as coded "{{trigger_code}}". Do not re-open that decision.
- Judge only what the passage actually says; do not speculate about the student.
- Write your reasoning first, in one or two sentences, then give your answer.
- Answer only through the provided response schema.

### USER
{{context_block}}Passage, already coded {{assigned_codes}}:
"""
{{unit_text}}
"""

Choose exactly one of: {{choice_list}}.
