// CPET Report Template for cardiometR
// Bilingual support (English/French)

// Document setup
#set document(
  title: "{{title}}",
  author: "{{institution}}"
)

// Color definitions - UdeM brand colors
#let primary = rgb("#0054A6")      // UdeM Blue
#let accent = rgb("#00A3E0")       // Light blue accent
#let success = rgb("#10B981")      // Green for positive values
#let warning = rgb("#F59E0B")      // Amber for warnings
#let danger = rgb("#EF4444")       // Red for concerns

#set page(
  paper: "a4",
  margin: (top: 3.2cm, bottom: 2cm, left: 2cm, right: 2cm),
  header: [
    #grid(
      columns: (auto, 1fr),
      gutter: 1em,
      align(left + horizon)[
        #box[
          {{#if logo_path}}
          #box(
            clip: true,
            height: 1.4cm,
            image("{{logo_path}}", height: 1.4cm)
          )
          {{/if}}
          {{#if lab_logo_path}}
          #h(0.5em)
          #box(
            clip: true,
            height: 1.4cm,
            image("{{lab_logo_path}}", height: 1.4cm)
          )
          {{/if}}
        ]
      ],
      align(right + horizon)[
        #stack(
          dir: ttb,
          spacing: 0.4em,
          text(weight: "bold", size: 9pt, fill: primary)[{{institution}}],
          {{#if lab_name}}
          {{#if lab_url}}
          link("{{lab_url}}")[#text(size: 8pt, fill: accent)[{{lab_name}}]],
          {{else}}
          text(size: 8pt, fill: accent)[{{lab_name}}],
          {{/if}}
          {{/if}}
          text(size: 8pt, fill: luma(100))[{{report_date}}]
        )
      ]
    )
    #v(0.3em)
    #line(length: 100%, stroke: 1.5pt + primary)
  ],
  footer: [
    #line(length: 100%, stroke: 0.5pt + luma(200))
    #v(0.3em)
    #set text(size: 8pt, fill: luma(120))
    #grid(
      columns: (1fr, 1fr, 1fr),
      align(left)[{{footer_left}}],
      align(center)[#context counter(page).display("1 / 1", both: true)],
      align(right)[_cardiometR_]
    )
  ]
)

// Use modern professional fonts with fallbacks
#set text(
  font: ("Inter", "Helvetica Neue", "Arial", "sans-serif"),
  size: 10pt
)

// Readable subscripts/superscripts (default 0.6em is too small in small text)
#set sub(size: 0.8em)
#set super(size: 0.8em)

// Monospace for data values
#show raw: set text(font: ("SF Mono", "Menlo", "Monaco", "monospace"))

#set heading(numbering: none)

#show heading.where(level: 1): it => [
  #set text(size: 13pt, weight: "bold", fill: primary)
  #block(above: 1.2em, below: 0.6em)[
    #box(width: 4pt, height: 1em, fill: primary, baseline: 20%)
    #h(0.5em)
    #it.body
  ]
]

#show heading.where(level: 2): it => [
  #set text(size: 11pt, weight: "semibold", fill: luma(60))
  #block(above: 1em, below: 0.5em)[#it]
]

// Title block - sleek gradient header
#block(
  fill: gradient.linear(primary, primary.darken(30%), angle: 135deg),
  inset: (x: 2em, y: 1.5em),
  radius: 8pt,
  width: 100%,
  stroke: none
)[
  #align(center)[
    #text(size: 22pt, weight: "bold", fill: white, tracking: 0.5pt)[
      {{title}}
    ]
    #v(0.5em)
    #text(size: 12pt, fill: white.transparentize(20%), style: "italic")[
      {{subtitle}}
    ]
  ]
]

#v(0.8em)

// Patient and Test Information - Combined side by side
#grid(
  columns: (1fr, 1fr),
  gutter: 1.5em,
  // Patient Info
  [
    #block(
      stroke: (left: 3pt + primary),
      inset: (left: 1em, y: 0.8em, right: 0.5em),
      width: 100%
    )[
      #text(size: 11pt, weight: "bold", fill: primary)[{{section_patient}}]
      #v(0.5em)
      #table(
        columns: (7em, 1fr),
        stroke: none,
        inset: (x: 0pt, y: 4pt),
        [#text(fill: luma(80))[{{label_name}}]], [#text(weight: "semibold")[{{patient_name}}]],
        [#text(fill: luma(80))[{{label_id}}]], [{{patient_id}}],
        [#text(fill: luma(80))[{{label_age}} / {{label_sex}}]], [{{patient_age}} {{label_years}} / {{patient_sex}}],
        [#text(fill: luma(80))[{{label_height}} / {{label_weight}}]], [{{patient_height}} cm / {{patient_weight}} kg],
        [#text(fill: luma(80))[{{label_bmi}}]], [{{patient_bmi}} kg/m#super[2]],
        [#text(fill: luma(80))[{{label_sport}}]], [{{patient_sport}}],
      )
    ]
  ],
  // Test Info
  [
    #block(
      stroke: (left: 3pt + accent),
      inset: (left: 1em, y: 0.8em, right: 0.5em),
      width: 100%
    )[
      #text(size: 11pt, weight: "bold", fill: accent)[{{section_test}}]
      #v(0.5em)
      #table(
        columns: (7em, 1fr),
        stroke: none,
        inset: (x: 0pt, y: 4pt),
        [#text(fill: luma(80))[{{label_test_date}}]], [#text(weight: "semibold")[{{test_date}}]],
        [#text(fill: luma(80))[{{label_protocol}}]], [{{test_protocol}}],
        [#text(fill: luma(80))[{{label_device}}]], [{{test_device}}],
        [#text(fill: luma(80))[{{label_duration}}]], [{{test_duration}}],
        [#text(fill: luma(80))[{{label_technician}}]], [{{test_technician}}],
        [#text(fill: luma(80))[{{label_reason}}]], [{{test_reason}}],
      )
    ]
  ]
)

#v(0.8em)

// Pre-Test Conditions (optional)
{{#if has_pretest_conditions}}
#block(
  stroke: (left: 3pt + luma(180)),
  inset: (left: 1em, y: 0.8em, right: 0.5em),
  width: 100%
)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[🍽],
    text(size: 11pt, weight: "bold", fill: luma(60))[{{section_pretest}}]
  )
  #v(0.5em)
  #grid(
    columns: (1fr, 1fr, 1fr, 1fr),
    gutter: 1em,
    // Nutritional State
    [
      #text(size: 8pt, fill: luma(100))[{{label_nutritional_state}}]
      #v(0.2em)
      #text(weight: "semibold")[{{nutritional_state}}]
      {{#if last_meal_hours}}
      #text(size: 8pt, fill: luma(100))[ ({{last_meal_hours}}{{label_hours_ago}})]
      {{/if}}
    ],
    // Fatigue State
    [
      #text(size: 8pt, fill: luma(100))[🔋 {{label_fatigue_state}}]
      #v(0.2em)
      #text(weight: "semibold")[{{fatigue_state}}]
    ],
    // Medications
    [
      #text(size: 8pt, fill: luma(100))[💊 {{label_medications}}]
      #v(0.2em)
      #text(weight: "semibold")[{{medication_list}}]
    ],
    // Caffeine
    [
      #text(size: 8pt, fill: luma(100))[☕ {{label_caffeine}}]
      #v(0.2em)
      {{#if caffeine_intake}}
      #text(weight: "semibold")[{{caffeine_mg}} mg]
      {{else}}
      #text(weight: "semibold")[—]
      {{/if}}
    ]
  )
]
#v(0.8em)
{{/if}}

// Protocol Details (optional)
{{#if has_protocol_details}}
#block(
  stroke: (left: 3pt + luma(180)),
  inset: (left: 1em, y: 0.8em, right: 0.5em),
  width: 100%
)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[⚙️],
    text(size: 11pt, weight: "bold", fill: luma(60))[{{section_protocol_details}}]
  )
  #v(0.5em)
  #grid(
    columns: (1fr, 1fr),
    gutter: 1.5em,
    // Protocol Parameters
    [
      #table(
        columns: (8em, 1fr),
        stroke: none,
        inset: (x: 0pt, y: 3pt),
        [#text(size: 9pt, fill: luma(100))[{{label_modality}}]], [#text(weight: "semibold")[{{protocol_modality_label}}]],
        [#text(size: 9pt, fill: luma(100))[{{label_starting_intensity}}]], [#text(weight: "semibold")[{{starting_intensity}} {{intensity_unit}}]],
        [#text(size: 9pt, fill: luma(100))[{{label_increment}}]], [#text(weight: "semibold")[{{increment_size}} {{intensity_unit}}]],
        [#text(size: 9pt, fill: luma(100))[{{label_stage_duration}}]], [#text(weight: "semibold")[{{stage_duration_s}} s]],
        {{#if data_type}}
        [#text(size: 9pt, fill: luma(100))[{{label_data_type}}]], [#text(weight: "semibold")[{{data_type}}]],
        {{/if}}
      )
    ],
    // Equipment
    [
      #table(
        columns: (auto, 1fr),
        stroke: none,
        inset: (x: 0pt, y: 3pt),
        {{#if equipment_model}}
        [#text(size: 9pt, fill: luma(100))[{{label_equipment}}]], [#text(weight: "semibold")[{{equipment_model}}]],
        {{/if}}
        {{#if analyzer_model}}
        [#text(size: 9pt, fill: luma(100))[{{label_analyzer}}]], [#text(weight: "semibold")[{{analyzer_model}}]],
        {{/if}}
      )
    ]
  )
]
#v(0.8em)
{{/if}}

// Peak Values - keep together on one page
#block(breakable: false)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[⛰️],
    heading(level: 1)[{{section_peak_values}}]
  )

  #grid(
    columns: (1fr, 1fr, 1fr),
    gutter: 1em,
    // VO2 Peak Card
    block(
      fill: primary.lighten(92%),
      stroke: (top: 3pt + primary),
      inset: 1em,
      radius: (bottom: 6pt),
      width: 100%
    )[
      #align(center)[
        #text(size: 28pt, weight: "bold", fill: primary)[{{vo2_peak_value}}]
        #v(0.2em)
        #text(size: 9pt, weight: "semibold")[{{label_vo2_peak}}]
        #v(0.3em)
        #box(
          fill: primary.lighten(80%),
          inset: (x: 0.6em, y: 0.3em),
          radius: 3pt
        )[#text(size: 8pt, fill: primary.darken(20%))[{{vo2_peak_percent}} {{label_predicted}}]]
      ]
    ],
    // HR Peak Card
    block(
      fill: danger.lighten(92%),
      stroke: (top: 3pt + danger),
      inset: 1em,
      radius: (bottom: 6pt),
      width: 100%
    )[
      #align(center)[
        #text(size: 28pt, weight: "bold", fill: danger)[{{hr_peak_value}}]
        #v(0.2em)
        #text(size: 9pt, weight: "semibold")[{{label_hr_peak}}]
        #v(0.3em)
        #box(
          fill: danger.lighten(80%),
          inset: (x: 0.6em, y: 0.3em),
          radius: 3pt
        )[#text(size: 8pt, fill: danger.darken(20%))[{{hr_peak_percent}} {{label_predicted}}]]
      ]
    ],
    // Power Peak Card
    block(
      fill: accent.lighten(92%),
      stroke: (top: 3pt + accent),
      inset: 1em,
      radius: (bottom: 6pt),
      width: 100%
    )[
      #align(center)[
        #text(size: 28pt, weight: "bold", fill: accent)[{{power_peak_value}}]
        #v(0.2em)
        #text(size: 9pt, weight: "semibold")[{{label_power_peak}}]
        #v(0.3em)
        #box(
          fill: accent.lighten(80%),
          inset: (x: 0.6em, y: 0.3em),
          radius: 3pt
        )[#text(size: 8pt, fill: accent.darken(20%))[{{power_peak_wkg}} W/kg]]
      ]
    ]
  )
]

#v(0.8em)

// Detailed Results Table - keep together on one page
#block(breakable: false)[
  == {{section_detailed_results}}

  #table(
    columns: (2.5fr, 1fr, 1fr, 1fr),
    inset: (x: 10pt, y: 8pt),
    stroke: none,
    fill: (col, row) => if row == 0 { primary } else if calc.odd(row) { luma(250) } else { white },
    align: (left, center, center, center),
    [#text(weight: "bold", fill: white)[{{label_parameter}}]],
    [#text(weight: "bold", fill: white)[{{label_value}}]],
    [#text(weight: "bold", fill: white)[{{label_predicted}}]],
    [#text(weight: "bold", fill: white)[% Pred.]],
    [{{label_vo2_peak_abs}}], [#text(weight: "semibold")[{{vo2_peak_abs}}]], [{{vo2_predicted}}], [#text(weight: "bold", fill: primary)[{{vo2_percent}}%]],
    [{{label_vo2_peak_rel}}], [#text(weight: "semibold")[{{vo2_peak_rel}}]], [{{vo2_rel_predicted}}], [#text(weight: "bold", fill: primary)[{{vo2_rel_percent}}%]],
    [{{label_ve_peak}}], [#text(weight: "semibold")[{{ve_peak}}]], [{{ve_predicted}}], [#text(weight: "bold", fill: primary)[{{ve_percent}}%]],
    [{{label_hr_peak_row}}], [#text(weight: "semibold")[{{hr_peak}}]], [{{hr_predicted}}], [#text(weight: "bold", fill: primary)[{{hr_percent}}%]],
    [{{label_rer_peak}}], [#text(weight: "semibold")[{{rer_peak}}]], [—], [—],
    [{{label_power_peak_row}}], [#text(weight: "semibold")[{{power_peak}}]], [{{power_predicted}}], [#text(weight: "bold", fill: primary)[{{power_percent}}%]],
    [{{label_o2_pulse}}], [#text(weight: "semibold")[{{o2_pulse}}]], [{{o2_pulse_predicted}}], [#text(weight: "bold", fill: primary)[{{o2_pulse_percent}}%]],
  )

  #v(0.3em)
  #text(size: 8pt, fill: luma(120))[
    {{predicted_values_note}}
  ]
]

#v(0.8em)

// Stage-by-Stage Results (optional)
{{#if has_stage_table}}
#grid(
  columns: (auto, 1fr),
  gutter: 0.5em,
  align(horizon)[📊],
  heading(level: 1)[{{section_stage_table}}]
)

{{stage_table}}

#v(0.8em)
{{/if}}

// Economy Metrics (optional) - keep together on one page
{{#if has_economy_metrics}}
#block(breakable: false)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[🏃],
    heading(level: 1)[{{section_economy}}]
  )

  #grid(
    columns: (1fr, 1fr),
    gutter: 1.5em,
    {{#if gross_efficiency}}
    // Cycling Gross Efficiency
    block(
      fill: accent.lighten(92%),
      stroke: (top: 3pt + accent),
      inset: 1em,
      radius: (bottom: 6pt),
      width: 100%
    )[
      #align(center)[
        #text(size: 28pt, weight: "bold", fill: accent)[{{gross_efficiency}}%]
        #v(0.2em)
        #text(size: 9pt, weight: "semibold")[{{label_gross_efficiency}}]
        #v(0.3em)
        #text(size: 8pt, fill: luma(100))[{{label_at_stage}} {{reference_stage}} ({{reference_power}} W)]
      ]
    ],
    {{/if}}
    {{#if running_economy}}
    // Running Economy
    block(
      fill: warning.lighten(92%),
      stroke: (top: 3pt + warning),
      inset: 1em,
      radius: (bottom: 6pt),
      width: 100%
    )[
      #align(center)[
        #text(size: 28pt, weight: "bold", fill: warning)[{{running_economy}}]
        #v(0.2em)
        #text(size: 9pt, weight: "semibold")[{{label_running_economy}}]
        #v(0.1em)
        #text(size: 8pt, fill: luma(100))[{{unit_ml_kg_km}}]
        #v(0.3em)
        #text(size: 8pt, fill: luma(100))[{{label_at_stage}} {{reference_stage}} ({{reference_speed}} km/h)]
      ]
    ],
    {{/if}}
  )
]

#v(0.8em)
{{/if}}

// Ventilatory Thresholds (only shown if detected) - keep together on one page
{{#if thresholds_detected}}
#block(breakable: false)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[🎯],
    heading(level: 1)[{{section_thresholds}}]
  )

  #table(
    columns: (2fr, 1fr, 1fr, 1fr, 1fr),
    inset: (x: 10pt, y: 8pt),
    stroke: none,
    fill: (col, row) => if row == 0 { warning } else if calc.odd(row) { luma(250) } else { white },
    align: (left, center, center, center, center),
    [#text(weight: "bold", fill: white)[{{label_threshold}}]],
    [#text(weight: "bold", fill: white)[VO#sub[2] (mL/min)]],
    [#text(weight: "bold", fill: white)[% VO#sub[2]max]],
    [#text(weight: "bold", fill: white)[{{label_hr_unit}}]],
    [#text(weight: "bold", fill: white)[{{label_power}} (W)]],
    [VT1 ({{label_aerobic}})], [{{vt1_vo2}}], [{{vt1_percent}}%], [{{vt1_hr}}], [{{vt1_power}}],
    [VT2 ({{label_anaerobic}})], [{{vt2_vo2}}], [{{vt2_percent}}%], [{{vt2_hr}}], [{{vt2_power}}],
  )

  #v(0.3em)
  #text(size: 8pt, fill: luma(120))[
    {{label_detection_method}}: {{threshold_method}} #h(1em) {{label_confidence}}: {{threshold_confidence}}
  ]
]

#v(0.8em)
{{/if}}

{{#if has_graphs}}
// Graphs
#grid(
  columns: (auto, 1fr),
  gutter: 0.5em,
  align(horizon)[📈],
  heading(level: 1)[{{section_graphs}}]
)

{{#if graph_panel}}
#figure(
  image("{{graph_panel}}", width: 100%),
  caption: [#text(size: 9pt)[{{caption_panel}}]]
)
{{/if}}

#v(0.5em)

{{#if graph_vslope}}
#grid(
  columns: (1fr, 1fr),
  gutter: 1em,
  [
    #figure(
      image("{{graph_vslope}}", width: 100%),
      caption: [#text(size: 9pt)[{{caption_vslope}}]]
    )
  ],
  {{#if graph_predicted}}
  [
    #figure(
      image("{{graph_predicted}}", width: 100%),
      caption: [#text(size: 9pt)[{{caption_predicted}}]]
    )
  ]
  {{/if}}
)
{{else}}
{{#if graph_predicted}}
#figure(
  image("{{graph_predicted}}", width: 100%),
  caption: [#text(size: 9pt)[{{caption_predicted}}]]
)
{{/if}}
{{/if}}

#v(0.8em)
{{/if}}

// Interpretation - Visual Summary - keep together on one page
#block(breakable: false)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[🧠],
    heading(level: 1)[{{section_interpretation}}]
  )

  #grid(
    columns: (1fr, 1fr, 1fr),
    gutter: 0.8em,
    // Aerobic Capacity Gauge
    block(
      stroke: (left: 4pt + {{aerobic_color}}),
      fill: luma(252),
      inset: (left: 1em, right: 0.8em, y: 0.8em),
      radius: (right: 6pt),
      width: 100%
    )[
      #text(size: 8pt, weight: "semibold", fill: luma(80))[{{section_aerobic_capacity}}]
      #v(0.3em)
      #block(
        fill: luma(225),
        radius: 3pt,
        width: 100%,
        height: 8pt
      )[
        #place(
          block(
            fill: {{aerobic_color}},
            radius: 3pt,
            width: {{aerobic_percent}}%,
            height: 8pt
          )
        )
      ]
      #v(0.3em)
      #grid(
        columns: (1fr, auto),
        text(size: 20pt, weight: "bold", fill: {{aerobic_color}})[{{vo2_percent}}%],
        align(right + bottom)[#text(size: 7pt, fill: luma(100))[{{aerobic_rating}}]]
      )
    ],
    // Cardiovascular Response Gauge
    block(
      stroke: (left: 4pt + {{cardiovascular_color}}),
      fill: luma(252),
      inset: (left: 1em, right: 0.8em, y: 0.8em),
      radius: (right: 6pt),
      width: 100%
    )[
      #text(size: 8pt, weight: "semibold", fill: luma(80))[{{section_cardiovascular}}]
      #v(0.3em)
      #block(
        fill: luma(225),
        radius: 3pt,
        width: 100%,
        height: 8pt
      )[
        #place(
          block(
            fill: {{cardiovascular_color}},
            radius: 3pt,
            width: {{cardiovascular_percent}}%,
            height: 8pt
          )
        )
      ]
      #v(0.3em)
      #grid(
        columns: (1fr, auto),
        text(size: 20pt, weight: "bold", fill: {{cardiovascular_color}})[{{hr_percent}}%],
        align(right + bottom)[#text(size: 7pt, fill: luma(100))[{{cardiovascular_rating}}]]
      )
    ],
    // Ventilatory Response Gauge
    block(
      stroke: (left: 4pt + {{ventilatory_color}}),
      fill: luma(252),
      inset: (left: 1em, right: 0.8em, y: 0.8em),
      radius: (right: 6pt),
      width: 100%
    )[
      #text(size: 8pt, weight: "semibold", fill: luma(80))[{{section_ventilatory}}]
      #v(0.3em)
      #block(
        fill: luma(225),
        radius: 3pt,
        width: 100%,
        height: 8pt
      )[
        #place(
          block(
            fill: {{ventilatory_color}},
            radius: 3pt,
            width: {{ventilatory_percent}}%,
            height: 8pt
          )
        )
      ]
      #v(0.3em)
      #grid(
        columns: (1fr, auto),
        text(size: 20pt, weight: "bold", fill: {{ventilatory_color}})[{{rer_peak}}],
        align(right + bottom)[#text(size: 7pt, fill: luma(100))[{{ventilatory_rating}}]]
      )
    ]
  )

  #v(0.6em)

  // Summary text
  #block(
    fill: primary.lighten(95%),
    stroke: (left: 3pt + primary),
    inset: (left: 1em, right: 1em, y: 0.8em),
    radius: (right: 4pt),
    width: 100%
  )[
    #text(size: 9pt)[{{interpretation_summary}}]
  ]
]

#v(0.8em)

// Clinical Notes (only if provided)
{{#if has_clinical_notes}}
#grid(
  columns: (auto, 1fr),
  gutter: 0.5em,
  align(horizon)[📝],
  heading(level: 1)[{{section_clinical_notes}}]
)

#block(
  fill: luma(252),
  stroke: 0.5pt + luma(220),
  inset: 1em,
  radius: 4pt,
  width: 100%
)[
  {{clinical_notes}}
]

#v(0.8em)
{{/if}}

#v(0.8em)

// Signature
#grid(
  columns: (1fr, 1fr),
  gutter: 3em,
  [
    #v(1.5em)
    #line(length: 90%, stroke: 0.5pt + luma(180))
    #v(0.3em)
    #text(size: 9pt, fill: luma(80))[{{label_technician_signature}}]
  ],
  [
    #v(0.8em)
    #text(size: 10pt)[{{signature_date}}]
    #v(0.3em)
    #line(length: 90%, stroke: 0.5pt + luma(180))
    #v(0.3em)
    #text(size: 9pt, fill: luma(80))[Date]
  ]
)

#v(1em)

#align(center)[
  #block(
    inset: (y: 0.5em),
    width: 100%
  )[
    #text(size: 7.5pt, fill: luma(140))[
      {{footer_disclaimer}}
    ]
  ]
]
