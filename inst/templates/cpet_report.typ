// CPET Report Template for cardiometR
// Bilingual support (English/French)

// Document setup
#set document(
  title: "{{title}}",
  author: "{{institution}}"
)

// Color definitions
#let primary = rgb("#2E86AB")
#let accent = rgb("#1B998B")
#let warning = rgb("#F77F00")
#let danger = rgb("#E94F37")

#set page(
  paper: "a4",
  margin: (top: 3.2cm, bottom: 2cm, left: 2cm, right: 2cm),
  header: [
    #grid(
      columns: (auto, 1fr),
      gutter: 1em,
      align(left + horizon)[
        {{#if logo_path}}
        #box(
          clip: true,
          height: 1.4cm,
          image("{{logo_path}}", height: 1.4cm)
        )
        {{/if}}
      ],
      align(right + horizon)[
        #stack(
          dir: ttb,
          spacing: 0.2em,
          text(weight: "bold", size: 10pt, fill: primary)[{{institution}}],
          {{#if lab_name}}
          text(size: 8pt, fill: luma(80))[{{lab_name}}],
          {{/if}}
          text(size: 9pt, fill: luma(100))[{{report_date}}]
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

// Title block
#block(
  fill: gradient.linear(primary, primary.darken(20%), angle: 90deg),
  inset: (x: 1.5em, y: 1.2em),
  radius: 6pt,
  width: 100%
)[
  #align(center)[
    #text(size: 20pt, weight: "bold", fill: white)[
      {{title}}
    ]
    #v(0.4em)
    #text(size: 11pt, fill: white.darken(10%))[
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
        columns: (auto, 1fr),
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
        columns: (auto, 1fr),
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

// Peak Values
= {{section_peak_values}}

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

#v(0.8em)

// Detailed Results Table
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

#v(0.8em)

// Ventilatory Thresholds
= {{section_thresholds}}

{{#if thresholds_detected}}
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
{{else}}
#block(
  fill: luma(248),
  inset: 1em,
  radius: 4pt
)[
  #text(fill: luma(100))[{{message_no_thresholds}}]
]
{{/if}}

#v(0.8em)

// Graphs
= {{section_graphs}}

{{#if graph_panel}}
#figure(
  image("{{graph_panel}}", width: 100%),
  caption: [#text(size: 9pt)[{{caption_panel}}]]
)
{{/if}}

#v(0.5em)

#grid(
  columns: (1fr, 1fr),
  gutter: 1em,
  {{#if graph_vslope}}
  [
    #figure(
      image("{{graph_vslope}}", width: 100%),
      caption: [#text(size: 9pt)[{{caption_vslope}}]]
    )
  ],
  {{/if}}
  {{#if graph_predicted}}
  [
    #figure(
      image("{{graph_predicted}}", width: 100%),
      caption: [#text(size: 9pt)[{{caption_predicted}}]]
    )
  ]
  {{/if}}
)

#v(0.8em)

// Interpretation - Visual Summary
= {{section_interpretation}}

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

#v(0.8em)

// Clinical Notes
= {{section_clinical_notes}}

#block(
  fill: luma(252),
  stroke: 0.5pt + luma(220),
  inset: 1em,
  radius: 4pt,
  width: 100%
)[
  {{clinical_notes}}
]

#v(1.5em)

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
    #v(1.5em)
    #line(length: 90%, stroke: 0.5pt + luma(180))
    #v(0.3em)
    #text(size: 9pt, fill: luma(80))[Date]
  ]
)

#v(1em)

{{#if bibliography}}
// Bibliography / References
#block(
  fill: luma(250),
  inset: (x: 1em, y: 0.8em),
  radius: 4pt,
  width: 100%
)[
  #text(size: 9pt, weight: "semibold", fill: luma(80))[{{section_references}}]
  #v(0.4em)
  #set text(size: 7.5pt, fill: luma(60))
  {{bibliography}}
]

#v(0.8em)
{{/if}}

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
