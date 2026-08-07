// CPET Report Template for cardiometR
// Bilingual support (English/French)
// Clinical layout: numbered sections, hairline rules, no decorative fills.

#set document(
  title: "{{title}}",
  author: "{{institution}}"
)

// Palette. Ink and rules are neutral; the accent carries the section
// numbering and the values that matter.
#let primary = rgb("#0054A6")
#let ink = rgb("#16181c")
#let body_ink = rgb("#26292e")
#let mid_ink = rgb("#3a414c")
#let muted = rgb("#5c6675")
#let faint = rgb("#8b95a3")
#let pale = rgb("#9aa4b2")
#let rule_strong = rgb("#c9d2dd")
#let rule_soft = rgb("#d8dee6")
#let rule_hair = rgb("#e6e9ee")
#let ok = rgb("#0e7a52")
#let vt1_col = rgb("#B45309")
#let vt2_col = rgb("#8C4A73")

#set page(
  paper: "us-letter",
  margin: (top: 2.9cm, bottom: 1.6cm, left: 1.5cm, right: 1.5cm),
  header: [
    #grid(
      columns: (auto, auto, auto, 1fr, auto),
      column-gutter: 4mm,
      align: horizon,
      {{#if logo_path}}
      image("{{logo_path}}", height: 6.5mm),
      {{else}}
      [],
      {{/if}}
      {{#if epic_logo_path}}
      image("{{epic_logo_path}}", height: 5.5mm),
      {{else}}
      [],
      {{/if}}
      {{#if lab_logo_path}}
      image("{{lab_logo_path}}", height: 7.5mm),
      {{else}}
      [],
      {{/if}}
      [],
      align(right)[
        #stack(
          dir: ttb,
          spacing: 0.6mm,
          text(size: 7.5pt, weight: 600)[{{institution}}],
          text(size: 7.5pt, fill: muted)[{{lab_name}}],
          text(size: 7pt, fill: faint)[{{running_header}}]
        )
      ]
    )
    #v(1.8mm)
    #line(length: 100%, stroke: 0.5pt + rule_strong)
  ],
  footer: [
    #line(length: 100%, stroke: 0.5pt + rule_strong)
    #v(1.2mm)
    #set text(size: 7.5pt, fill: faint)
    #grid(
      columns: (1fr, auto, 1fr),
      align(left)[{{footer_left}}],
      align(center)[#context counter(page).display("1 / 1", both: true)],
      align(right)[_cardiometR_]
    )
  ]
)

#set text(
  font: ("Inter", "Helvetica Neue", "Arial", "sans-serif"),
  size: 9.5pt,
  fill: ink
)
#set par(leading: 0.55em, justify: false)

// Readable subscripts/superscripts (default 0.6em is too small in small text)
#set sub(size: 0.8em)
#set super(size: 0.8em)

#show raw: set text(font: ("SF Mono", "Menlo", "Monaco", "monospace"))

// Numbered section rule. The counter keeps the numbers contiguous even
// when the operator switches a section off.
#let seccount = counter("cardiometr-section")
#let sec(title) = block(above: 4mm, below: 2mm, width: 100%)[
  #seccount.step()
  #grid(
    columns: (auto, auto, 1fr),
    column-gutter: 2.5mm,
    align: horizon,
    text(size: 7.5pt, weight: 600, fill: pale)[#context {
      let n = seccount.get().first()
      if n < 10 [0#n] else [#n]
    }],
    text(size: 9pt, weight: 700, tracking: 1.2pt, fill: primary)[#upper(title)],
    line(length: 100%, stroke: 0.5pt + rule_soft)
  )
]

// A key/value line in the identity band.
#let kv(k, v) = (
  text(size: 8pt, fill: muted)[#k],
  text(size: 8.5pt, weight: 600)[#v]
)

// A figure caption, set as running text rather than a numbered float.
#let caption(body) = text(size: 7.5pt, fill: muted)[#body]

// Table styling shared by every table in the report: open sides, one
// heavy rule above the header, hairlines between rows.
#let clinical_table(columns: (), align_spec: auto, ..cells) = table(
  columns: columns,
  align: align_spec,
  inset: (x: 3mm, y: 1.7mm),
  stroke: (x, y) => (
    top: if y == 0 { 1pt + ink } else if y == 1 { 0.5pt + ink } else { 0.4pt + rule_hair },
    bottom: 0.4pt + rule_hair
  ),
  ..cells
)

#let th(body) = text(size: 7.5pt, weight: 600, tracking: 0.6pt, fill: mid_ink)[#upper(body)]


// ══════ Title block ══════

#block(above: 0mm, below: 2.2mm, width: 100%)[
  #line(length: 100%, stroke: 2pt + primary)
  #v(2.2mm)
  #grid(
    columns: (1fr, auto),
    align: bottom,
    text(size: 7.5pt, weight: 700, tracking: 1.5pt, fill: primary)[{{report_kicker}}],
    text(size: 7.5pt, fill: muted)[{{record_line}}]
  )
  #v(1mm)
  #text(size: 17pt, weight: 700)[{{patient_name}}]
  #v(0.6mm)
  #text(size: 9.5pt, fill: muted, style: "italic")[{{test_summary_line}}]
]

// ══════ Identity band: participant, pre-test, protocol ══════

#block(width: 100%, above: 2.8mm, below: 2.8mm, stroke: (top: 0.5pt + rule_soft, bottom: 0.5pt + rule_soft), inset: (y: 2mm))[
  #grid(
    columns: (1fr, 1fr, 1fr),
    column-gutter: 0mm,
    block(inset: (right: 5mm))[
      #grid(
        columns: (22mm, 1fr),
        row-gutter: 1mm,
        column-gutter: 2mm,
        ..kv([{{label_id}}], [{{patient_id}}]),
        ..kv([{{label_age}} / {{label_sex}}], [{{patient_age}} {{label_years}} / {{patient_sex}}]),
        ..kv([{{label_height}} / {{label_weight}}], [{{patient_height}} cm / {{patient_weight}} kg]),
        ..kv([{{label_bmi}}], [{{patient_bmi}} kg/m#super[2]]),
        ..kv([{{label_sport}}], [{{patient_sport}}])
      )
    ],
    block(inset: (left: 5mm, right: 5mm), stroke: (left: 0.5pt + rule_hair))[
      #text(size: 7pt, weight: 700, tracking: 1.1pt, fill: faint)[#upper[{{section_pretest}}]]
      #v(1mm)
      #grid(
        columns: (24mm, 1fr),
        row-gutter: 1mm,
        column-gutter: 2mm,
        {{#if has_pretest_conditions}}
        ..kv([{{label_nutritional_state}}], [{{nutritional_state}}]),
        ..kv([{{label_fatigue_state}}], [{{fatigue_state}}]),
        ..kv([{{label_medications}}], [{{medication_list}}]),
        {{#if caffeine_intake}}
        ..kv([{{label_caffeine}}], [{{caffeine_mg}} mg]),
        {{else}}
        ..kv([{{label_caffeine}}], [—]),
        {{/if}}
        {{else}}
        ..kv([—], [—]),
        {{/if}}
      )
    ],
    block(inset: (left: 5mm), stroke: (left: 0.5pt + rule_hair))[
      #text(size: 7pt, weight: 700, tracking: 1.1pt, fill: faint)[#upper[{{section_protocol_details}}]]
      #v(1mm)
      #grid(
        columns: (24mm, 1fr),
        row-gutter: 1mm,
        column-gutter: 2mm,
        ..kv([{{label_modality}}], [{{protocol_modality_label}}]),
        ..kv([{{label_starting_intensity}}], [{{starting_intensity_display}}]),
        ..kv([{{label_increment}}], [{{increment_size_display}}]),
        {{#if equipment_model}}
        ..kv([{{label_equipment}}], [{{equipment_model}}]),
        {{else}}
        ..kv([{{label_stage_duration}}], [{{stage_duration_display}}]),
        {{/if}}
      )
    ]
  )
]

// ══════ 01 Athlete profile ══════

{{#if has_athlete_profile}}
#block(breakable: false, width: 100%)[
  #sec[{{section_athlete_profile}}]
  #grid(
    columns: (1fr, 1fr, 1fr),
    block(inset: (right: 5mm, y: 1.5mm))[
      #text(size: 7.5pt, weight: 600, tracking: 0.5pt, fill: muted)[#upper[{{ap_card1_label}}]]
      #v(0.6mm)
      #text(size: 20pt, weight: 700)[{{ap_card1_value}}] #h(1.5mm) #text(size: 7.5pt, fill: faint)[{{ap_card1_unit}}]
      #v(0.6mm)
      #text(size: 7.5pt, weight: 600, fill: primary)[{{ap_card1_zline}}]
    ],
    block(inset: (left: 5mm, right: 5mm, y: 1.5mm), stroke: (left: 0.5pt + rule_hair))[
      #text(size: 7.5pt, weight: 600, tracking: 0.5pt, fill: muted)[#upper[{{ap_card2_label}}]]
      #v(0.6mm)
      #text(size: 20pt, weight: 700)[{{ap_card2_value}}] #h(1.5mm) #text(size: 7.5pt, fill: faint)[{{ap_card2_unit}}]
      #v(0.6mm)
      #text(size: 7.5pt, weight: 600, fill: primary)[{{ap_card2_zline}}]
    ],
    block(inset: (left: 5mm, y: 1.5mm), stroke: (left: 0.5pt + rule_hair))[
      #text(size: 7.5pt, weight: 600, tracking: 0.5pt, fill: muted)[#upper[{{ap_card3_label}}]]
      #v(0.6mm)
      #text(size: 20pt, weight: 700)[{{ap_card3_value}}] #h(1.5mm) #text(size: 7.5pt, fill: faint)[{{ap_card3_unit}}]
      #v(0.6mm)
      #text(size: 7.5pt, weight: 600, fill: primary)[{{ap_card3_zline}}]
    ]
  )
]
{{/if}}

// ══════ 02 Resting values ══════

{{#if has_resting}}
#block(breakable: false, width: 100%)[
  #sec[{{resting_title}}]
  #block(width: 100%, stroke: (top: 0.5pt + rule_soft, bottom: 0.5pt + rule_soft), inset: (y: 1.8mm))[
  // Parameters are not named `v`: that would shadow Typst's own #v()
  // spacing function inside the body.
  #let restcell(name, amount, unit) = align(center)[
    #text(size: 7pt, tracking: 0.4pt, fill: faint)[#upper[#name]]
    #v(0.3mm)
    #text(size: 12pt, weight: 700)[#amount]
    #v(0.3mm)
    #text(size: 7pt, fill: faint)[#unit]
  ]
  #grid(
    columns: (1fr, 1fr, 1fr, 1fr, 1fr, 1fr),
    restcell[VO#sub[2]][{{resting_vo2}}][mL/min],
    restcell[VO#sub[2]/kg][{{resting_vo2_kg}}][mL/kg/min],
    restcell[{{label_hr_unit}}][{{resting_hr}}][bpm],
    restcell[VE][{{resting_ve}}][L/min],
    restcell[{{label_rer_short}}][{{resting_rer}}][],
    restcell[{{resting_duration_label}}][{{resting_duration}}][m:ss]
    )
  ]
  #v(1mm)
  #caption[{{resting_caption}}]
]
{{/if}}

// ══════ 03 Test validity ══════

{{#if has_validity}}
#block(breakable: false, width: 100%)[
  #sec[{{section_validity}}]
  #block(width: 100%, stroke: 0.5pt + rule_soft, inset: (x: 3.5mm, y: 2.5mm))[
  #grid(
    columns: (auto, 1fr),
    column-gutter: 4mm,
    align: top,
    box(baseline: -1.2mm)[#circle(radius: 1.3mm, fill: {{validity_color}}, stroke: none)],
    [
      #text(size: 9pt, weight: 700, fill: {{validity_color}})[{{validity_title}}]
      #v(0.8mm)
      #text(size: 8.5pt, fill: mid_ink)[{{validity_detail}}]
      ]
    )
  ]
]
{{/if}}

// ══════ 04 VO2-power relationship ══════

{{#if graph_slope}}
#block(breakable: false, width: 100%)[
  #sec[{{section_vo2_power_slope}}]
  #image("{{graph_slope}}", width: 100%)
  #v(1mm)
  #caption[{{caption_vo2_power_slope}} {{slope_caption}}]
]
{{/if}}

// ══════ 05 Detailed results ══════

#block(breakable: false, width: 100%)[
  #sec[{{section_detailed_results}}]
  #clinical_table(
    columns: (2.6fr, 1fr, 1fr, 1fr),
    align_spec: (left, center, center, center),
    th[{{label_parameter}}], th[{{label_value}}], th[{{label_predicted}}], th[% {{label_predicted}}],
    [{{label_vo2_peak_abs}}], [#text(weight: 600)[{{vo2_peak_abs}}]], [#text(fill: muted)[{{vo2_predicted}}]], [#text(weight: 700, fill: primary)[{{vo2_percent}} %]],
    [{{label_vo2_peak_rel}}], [#text(weight: 600)[{{vo2_peak_rel}}]], [#text(fill: muted)[{{vo2_rel_predicted}}]], [#text(weight: 700, fill: primary)[{{vo2_rel_percent}} %]],
    [{{label_ve_peak}}], [#text(weight: 600)[{{ve_peak}}]], [#text(fill: muted)[{{ve_predicted}}]], [#text(weight: 700, fill: primary)[{{ve_percent}} %]],
    [{{label_hr_peak_row}}], [#text(weight: 600)[{{hr_peak}}]], [#text(fill: muted)[{{hr_predicted}}]], [#text(weight: 700, fill: primary)[{{hr_percent}} %]],
    [{{label_rer_peak}}], [#text(weight: 600)[{{rer_peak}}]], [#text(fill: muted)[—]], [#text(fill: muted)[—]],
    [{{label_power_peak_row}}], [#text(weight: 600)[{{power_peak}}]], [#text(fill: muted)[{{power_predicted}}]], [#text(weight: 700, fill: primary)[{{power_percent}}]],
    [{{label_o2_pulse}}], [#text(weight: 600)[{{o2_pulse}}]], [#text(fill: muted)[{{o2_pulse_predicted}}]], [#text(weight: 700, fill: primary)[{{o2_pulse_percent}} %]],
  )
  #v(1mm)
  #caption[{{predicted_values_note}}]
]

// ══════ 06 Population norms ══════

{{#if has_population_norms}}
#block(breakable: false, width: 100%)[
  #sec[{{pn_section_title}}]
  #caption[_{{pn_description}} — {{pn_citation_short}}_]
  #v(1.5mm)
  #clinical_table(
    columns: (2fr, 1fr, 1fr, 1.3fr, 1.3fr),
    align_spec: (left, center, center, center, center),
    th[{{pn_label_metric}}], th[{{pn_label_patient}}], th[{{pn_label_mean}}],
    th[{{pn_label_band}}], th[{{pn_label_zpct}}],
    {{pn_rows_content}}
  )
  #v(1mm)
  #caption[{{pn_sd_note}}]
]
{{#if graph_zstrip}}
#block(breakable: false, width: 100%)[
  #v(2mm)
  #image("{{graph_zstrip}}", width: 100%)
  #v(1mm)
  #caption[{{caption_zscore_strip}}]
]
{{/if}}
{{/if}}

// ══════ 07 Stage-by-stage results ══════

{{#if has_stage_table}}
#sec[{{section_stage_table}}]
{{stage_table}}
{{/if}}

// ══════ Economy (optional, kept from the analysis) ══════

{{#if has_economy_metrics}}
#block(breakable: false, width: 100%)[
  #sec[{{section_economy}}]
  #grid(
    columns: (1fr, 1fr),
    {{#if gross_efficiency}}
    block(inset: (right: 5mm, y: 1.5mm))[
      #text(size: 7.5pt, weight: 600, tracking: 0.5pt, fill: muted)[#upper[{{label_gross_efficiency}}]]
      #v(0.6mm)
      #text(size: 15pt, weight: 700)[{{gross_efficiency}} %]
      #v(0.6mm)
      #caption[{{label_at_stage}} {{reference_stage}} — {{reference_power}}]
    ],
    {{/if}}
    {{#if running_economy}}
    block(inset: (left: 5mm, y: 1.5mm), stroke: (left: 0.5pt + rule_hair))[
      #text(size: 7.5pt, weight: 600, tracking: 0.5pt, fill: muted)[#upper[{{label_running_economy}}]]
      #v(0.6mm)
      #text(size: 15pt, weight: 700)[{{running_economy}}] #h(1.5mm) #text(size: 7.5pt, fill: faint)[{{unit_ml_kg_km}}]
      #v(0.6mm)
      #caption[{{label_at_stage}} {{reference_stage}} — {{reference_speed}}]
    ],
    {{/if}}
  )
]
{{/if}}

// ══════ 08 Ventilatory thresholds ══════

{{#if has_vt_table}}
#block(breakable: false, width: 100%)[
  #sec[{{section_thresholds}}]
  #clinical_table(
    columns: (1.6fr, 1fr, 1fr, 1fr, 1fr),
    align_spec: (left, center, center, center, center),
    th[{{label_threshold}}], th[VO#sub[2] (mL/min)], th[% VO#sub[2]],
    th[{{label_hr_unit}}], th[{{label_power}} (W)],
    [#text(weight: 600, fill: vt1_col)[{{label_aerobic}}]], [{{vt1_vo2}}], [{{vt1_percent}} %], [{{vt1_hr}}], [{{vt1_power}}],
    [#text(weight: 600, fill: vt2_col)[{{label_anaerobic}}]], [{{vt2_vo2}}], [{{vt2_percent}} %], [{{vt2_hr}}], [{{vt2_power}}],
  )
  #v(1mm)
  #caption[{{label_detection_method}} : {{threshold_method}} · {{label_confidence}} : {{threshold_confidence}}]
]
{{/if}}

{{#if graph_vslope}}
#block(breakable: false, width: 100%)[
  #v(1.5mm)
  #grid(
    columns: (1fr, 1fr),
    column-gutter: 6mm,
    [
      #image("{{graph_vslope}}", width: 100%)
      #v(1mm)
      #caption[{{caption_vslope}}]
    ],
    {{#if graph_predicted}}
    [
      #image("{{graph_predicted}}", width: 100%)
      #v(1mm)
      #caption[{{caption_predicted}}]
    ]
    {{else}}
    []
    {{/if}}
  )
]
{{/if}}

// ══════ 09 Interpretation ══════

#block(breakable: false, width: 100%)[
  #sec[{{section_interpretation}}]
  #let gauge(title, pct, value, rating, col, first) = block(
    inset: if first { (right: 5mm, y: 1.5mm) } else { (left: 5mm, right: 5mm, y: 1.5mm) },
    stroke: if first { none } else { (left: 0.5pt + rule_hair) }
  )[
    #text(size: 7pt, weight: 600, tracking: 0.7pt, fill: muted)[#upper[#title]]
    #v(1mm)
    #block(fill: rgb("#eceff3"), width: 100%, height: 1.6mm)[
      #place(block(fill: col, width: pct, height: 1.6mm))
    ]
    #v(1mm)
    #grid(
      columns: (1fr, auto),
      align: bottom,
      text(size: 15pt, weight: 700)[#value],
      text(size: 7.5pt, weight: 600, fill: ok)[#rating]
    )
  ]
  #grid(
    columns: (1fr, 1fr, 1fr),
    gauge([{{section_aerobic_capacity}}], {{aerobic_percent}}%, [{{vo2_percent}} %], [{{aerobic_rating}}], {{aerobic_color}}, true),
    gauge([{{section_cardiovascular}}], {{cardiovascular_percent}}%, [{{hr_percent}} %], [{{cardiovascular_rating}}], {{cardiovascular_color}}, false),
    gauge([{{section_ventilatory}}], {{ventilatory_percent}}%, [{{rer_peak}}], [{{ventilatory_rating}}], {{ventilatory_color}}, false)
  )
  #v(2mm)
  #block(
    width: 100%,
    stroke: (top: 0.5pt + rule_soft, bottom: 0.5pt + rule_soft),
    inset: (x: 0.5mm, y: 2.5mm)
  )[
    #text(size: 8.5pt, fill: body_ink)[{{interpretation_summary}}]
  ]
]

// ══════ 10 Nine-panel display ══════

{{#if graph_panel}}
#block(breakable: false, width: 100%)[
  #sec[{{section_graphs}}]
  #image("{{graph_panel}}", width: 100%)
  #v(1mm)
  #caption[{{caption_panel}}]
]
{{/if}}

// ══════ Longitudinal comparison (optional) ══════

{{#if has_longitudinal}}
{{#if graph_longitudinal}}
#block(breakable: false, width: 100%)[
  #sec[{{section_longitudinal}}]
  #image("{{graph_longitudinal}}", width: 100%)
  #v(1mm)
  #caption[{{caption_longitudinal}}]
]
{{/if}}
{{/if}}

// ══════ 11 Estimates and caveats ══════

{{#if has_estimates_caveats}}
#block(breakable: false, width: 100%)[
  #sec[{{estimates_and_caveats}}]
  #grid(
    columns: (1.4fr, 1fr),
    column-gutter: 6mm,
    align: top,
    [
      {{#if has_vt_block}}
      #clinical_table(
        columns: (1.6fr, 1fr, 1fr, 1fr),
        align_spec: (left, center, center, center),
        th[{{metric}}], th[{{low}}], th[{{high}}], th[{{point}}],
        {{vt_rows_content}}
      )
      #v(1mm)
      #caption[_{{vt_caveat}}_]
      {{/if}}
    ],
    [
      {{#if has_ftp_block}}
      #block(inset: (left: 5mm), stroke: (left: 0.5pt + rule_soft))[
        #text(size: 7pt, weight: 600, tracking: 0.7pt, fill: muted)[#upper[{{ftp_range}}]]
        #v(1mm)
        #text(size: 8.5pt, fill: mid_ink)[0,72–0,77 × PAM]
        #v(1mm)
        #text(size: 15pt, weight: 700)[{{ftp_low}}–{{ftp_high}} W]
        #v(1.5mm)
        #caption[_{{ftp_caveat}}_]
      ]
      {{/if}}
    ]
  )
]
{{/if}}

// ══════ 12 Clinical notes ══════

{{#if has_clinical_notes}}
#sec[{{section_clinical_notes}}]
#block(
  width: 100%,
  stroke: (top: 0.5pt + rule_soft, bottom: 0.5pt + rule_soft),
  inset: (x: 0.5mm, y: 2.5mm)
)[
  #text(size: 9pt, fill: body_ink)[{{clinical_notes}}]
]
{{/if}}

// ══════ 13 Analysis parameters ══════

{{#if has_analysis_params}}
#block(breakable: false, width: 100%)[
  #sec[{{section_analysis_params}}]
  #grid(
    columns: (1fr, 1fr),
    row-gutter: 1.4mm,
    column-gutter: 8mm,
    {{analysis_params_content}}
  )
]
{{/if}}

// ══════ Signature ══════

#v(6mm)
#grid(
  columns: (1fr, 1fr),
  column-gutter: 14mm,
  [
    #v(10mm)
    #line(length: 100%, stroke: 0.5pt + pale)
    #v(1mm)
    #text(size: 8pt, fill: muted)[{{label_technician_signature}}]
  ],
  [
    #v(10mm)
    #text(size: 9.5pt)[{{signature_date}}]
    #v(1mm)
    #line(length: 100%, stroke: 0.5pt + pale)
    #v(1mm)
    #text(size: 8pt, fill: muted)[Date]
  ]
)

#v(4mm)
#align(center)[
  #text(size: 7pt, fill: pale)[{{footer_disclaimer}}]
]
