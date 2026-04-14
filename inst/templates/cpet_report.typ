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
      columns: (1fr, 1fr),
      gutter: 1em,
      align(left + horizon)[
        #grid(
          columns: (auto, auto),
          gutter: 0.8em,
          align(left + horizon)[
            {{#if logo_path}}
            #box(
              height: 1.35cm,
            )[
              #align(center + horizon)[
                #image("{{logo_path}}", height: 1.15cm)
              ]
            ]
            {{/if}}
          ],
          align(left + horizon)[
            {{#if lab_logo_path}}
            #box(
              height: 1.35cm,
            )[
              #align(center + horizon)[
                #image("{{lab_logo_path}}", height: 1.30cm)
              ]
            ]
            {{/if}}
          ]
        )
      ],
      align(right + top)[
        #stack(
          dir: ttb,
          spacing: 0.4em,
          text(weight: "bold", size: 9pt, fill: primary)[{{institution_line1}}],
          {{#if institution_line2}}
          text(weight: "bold", size: 9pt, fill: primary)[{{institution_line2}}],
          {{/if}}
          {{#if lab_name_line1}}
          {{#if lab_url}}
          link("{{lab_url}}")[#text(size: 8pt, fill: accent)[{{lab_name_line1}}]],
          {{#if lab_name_line2}}
          link("{{lab_url}}")[#text(size: 8pt, fill: accent)[{{lab_name_line2}}]],
          {{/if}}
          {{else}}
          text(size: 8pt, fill: accent)[{{lab_name_line1}}],
          {{#if lab_name_line2}}
          text(size: 8pt, fill: accent)[{{lab_name_line2}}],
          {{/if}}
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

// Patient Information
#block(
  stroke: (left: 3pt + primary),
  inset: (left: 1em, y: 0.8em, right: 0.5em),
  width: 100%
)[
  #text(size: 11pt, weight: "bold", fill: primary)[{{section_patient}}]
  #v(0.5em)
  #grid(
    columns: (1fr, 1fr),
    gutter: 1.5em,
    table(
      columns: (7em, 1fr),
      stroke: none,
      inset: (x: 0pt, y: 4pt),
      [#text(fill: luma(80))[{{label_name}}]], [#text(weight: "semibold")[{{patient_name}}]],
      [#text(fill: luma(80))[{{label_id}}]], [{{patient_id}}],
      [#text(fill: luma(80))[{{label_age}} / {{label_sex}}]], [{{patient_age}} {{label_years}} / {{patient_sex}}],
    ),
    table(
      columns: (7em, 1fr),
      stroke: none,
      inset: (x: 0pt, y: 4pt),
      [#text(fill: luma(80))[{{label_height}} / {{label_weight}}]], [{{patient_height}} cm / {{patient_weight}} kg],
      [#text(fill: luma(80))[{{label_bmi}}]], [{{patient_bmi}} kg/m#super[2]],
      [#text(fill: luma(80))[{{label_sport}}]], [{{patient_sport}}],
    )
  )
]

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
        [#text(size: 9pt, fill: luma(100))[{{label_starting_intensity}}]], [#text(weight: "semibold")[{{starting_intensity_display}}]],
        [#text(size: 9pt, fill: luma(100))[{{label_increment}}]], [#text(weight: "semibold")[{{increment_size_display}}]],
        [#text(size: 9pt, fill: luma(100))[{{label_stage_duration}}]], [#text(weight: "semibold")[{{stage_duration_display}}]],
        {{#if data_type}}
        [#text(size: 9pt, fill: luma(100))[{{label_data_type}}]], [#text(weight: "semibold")[{{data_type}}]],
        {{/if}}
      )
    ],
    // Equipment
    [
      #table(
        columns: (8em, 1fr),
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

// Athlete Profile (Phase 7) — three-column headline
{{#if has_athlete_profile}}
#block(breakable: false)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[🚴],
    heading(level: 1)[{{section_athlete_profile}}]
  )

  #grid(
    columns: (1fr, 1fr, 1fr),
    gutter: 1em,
    block(
      fill: primary.lighten(94%),
      stroke: (top: 3pt + primary),
      inset: 0.9em,
      radius: (bottom: 6pt),
      width: 100%
    )[
      #align(center)[
        #text(size: 9pt, weight: "semibold", fill: luma(80))[{{ap_card1_label}}]
        #v(0.2em)
        #text(size: 22pt, weight: "bold", fill: primary)[{{ap_card1_value}}]
        #v(0.1em)
        #text(size: 8pt, fill: luma(110))[{{ap_card1_unit}}]
        #v(0.3em)
        #text(size: 8pt)[{{ap_card1_zline}}]
      ]
    ],
    block(
      fill: accent.lighten(94%),
      stroke: (top: 3pt + accent),
      inset: 0.9em,
      radius: (bottom: 6pt),
      width: 100%
    )[
      #align(center)[
        #text(size: 9pt, weight: "semibold", fill: luma(80))[{{ap_card2_label}}]
        #v(0.2em)
        #text(size: 22pt, weight: "bold", fill: accent)[{{ap_card2_value}}]
        #v(0.1em)
        #text(size: 8pt, fill: luma(110))[{{ap_card2_unit}}]
        #v(0.3em)
        #text(size: 8pt)[{{ap_card2_zline}}]
      ]
    ],
    block(
      fill: warning.lighten(94%),
      stroke: (top: 3pt + warning),
      inset: 0.9em,
      radius: (bottom: 6pt),
      width: 100%
    )[
      #align(center)[
        #text(size: 9pt, weight: "semibold", fill: luma(80))[{{ap_card3_label}}]
        #v(0.2em)
        #text(size: 22pt, weight: "bold", fill: warning)[{{ap_card3_value}}]
        #v(0.1em)
        #text(size: 8pt, fill: luma(110))[{{ap_card3_unit}}]
        #v(0.3em)
        #text(size: 8pt)[{{ap_card3_zline}}]
      ]
    ]
  )
]

{{#if has_resting}}
#v(0.6em)
#block(breakable: false)[
  #text(size: 10pt, weight: "semibold")[{{resting_title}}]
  #v(0.3em)
  #grid(
    columns: (1fr, 1fr, 1fr, 1fr, 1fr, 1fr),
    gutter: 0.5em,
    align(center)[
      #text(size: 8pt, fill: luma(110))[VO2]
      #v(-0.3em)
      #text(size: 14pt, weight: "bold")[{{resting_vo2}}]
      #text(size: 8pt)[ mL/min]
    ],
    align(center)[
      #text(size: 8pt, fill: luma(110))[VO2/kg]
      #v(-0.3em)
      #text(size: 14pt, weight: "bold")[{{resting_vo2_kg}}]
      #text(size: 8pt)[ mL/kg/min]
    ],
    align(center)[
      #text(size: 8pt, fill: luma(110))[HR]
      #v(-0.3em)
      #text(size: 14pt, weight: "bold")[{{resting_hr}}]
      #text(size: 8pt)[ bpm]
    ],
    align(center)[
      #text(size: 8pt, fill: luma(110))[VE]
      #v(-0.3em)
      #text(size: 14pt, weight: "bold")[{{resting_ve}}]
      #text(size: 8pt)[ L/min]
    ],
    align(center)[
      #text(size: 8pt, fill: luma(110))[RER]
      #v(-0.3em)
      #text(size: 14pt, weight: "bold")[{{resting_rer}}]
    ],
    align(center)[
      #text(size: 8pt, fill: luma(110))[{{resting_duration_label}}]
      #v(-0.3em)
      #text(size: 14pt, weight: "bold")[{{resting_duration}}]
    ]
  )
  #v(0.2em)
  #text(size: 8pt, fill: luma(110))[{{resting_caption}}]
]
{{/if}}

{{#if graph_slope}}
#v(0.6em)
#block(breakable: false)[
  == {{section_vo2_power_slope}}
  #figure(
    image("{{graph_slope}}", width: 100%),
    caption: [#text(size: 9pt)[{{caption_vo2_power_slope}} {{slope_caption}}]]
  )
]
{{/if}}

{{#if graph_zstrip}}
#v(0.6em)
#block(breakable: false)[
  == {{section_zscore_strip}}
  #figure(
    image("{{graph_zstrip}}", width: 100%),
    caption: [#text(size: 9pt)[{{caption_zscore_strip}}]]
  )
]
{{/if}}

#v(0.8em)
{{/if}}

// Detailed Results Table — keep together on one page
// (Formerly preceded by a 3-card "Peak Values" block that duplicated
// the front-page athlete profile; removed to avoid redundancy.)
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
    [{{label_power_peak_row}}], [#text(weight: "semibold")[{{power_peak}}]], [{{power_predicted}}], [#text(weight: "bold", fill: primary)[{{power_percent}}]],
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

// Longitudinal comparison (Phase 7)
{{#if has_longitudinal}}
{{#if graph_longitudinal}}
#block(breakable: false)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[📉],
    heading(level: 1)[{{section_longitudinal}}]
  )
  #figure(
    image("{{graph_longitudinal}}", width: 100%),
    caption: [#text(size: 9pt)[{{caption_longitudinal}}]]
  )
]
#v(0.8em)
{{/if}}
{{/if}}

// Estimates & Caveats (Phase 7)
{{#if has_estimates_caveats}}
#block(breakable: false)[
  #grid(
    columns: (auto, 1fr),
    gutter: 0.5em,
    align(horizon)[⚖️],
    heading(level: 1)[{{estimates_and_caveats}}]
  )

  // VT range
  {{#if has_vt_block}}
  == {{vt_range_title}}
  #block(
    fill: luma(252),
    stroke: (left: 3pt + warning),
    inset: (left: 1em, right: 1em, y: 0.7em),
    radius: (right: 4pt),
    width: 100%
  )[
    #text(size: 10pt, weight: "bold", fill: luma(60))[{{vt_range}}]
    #v(0.4em)
    #table(
      columns: (1.2fr, 1fr, 1fr, 1fr),
      inset: (x: 8pt, y: 5pt),
      stroke: none,
      fill: (col, row) => if row == 0 { luma(235) } else if calc.odd(row) { luma(250) } else { white },
      align: (left, center, center, center),
      [#text(weight: "bold")[{{metric}}]],
      [#text(weight: "bold")[{{low}}]],
      [#text(weight: "bold")[{{high}}]],
      [#text(weight: "bold")[{{point}}]],
      {{vt_rows_content}}
    )
    #v(0.2em)
    #text(size: 8pt, fill: luma(110), style: "italic")[{{vt_caveat}}]
  ]
  #v(0.6em)
  {{/if}}

  // FTP range
  {{#if has_ftp_block}}
  #block(
    fill: luma(252),
    stroke: (left: 3pt + accent),
    inset: (left: 1em, right: 1em, y: 0.7em),
    radius: (right: 4pt),
    width: 100%
  )[
    #text(size: 10pt, weight: "bold", fill: luma(60))[{{ftp_range}}]
    #v(0.3em)
    #text(size: 10pt)[0.72–0.77 × MAP = #text(weight: "bold")[{{ftp_low}}–{{ftp_high}} W]]
    #v(0.2em)
    #text(size: 8pt, fill: luma(110), style: "italic")[{{ftp_caveat}}]
  ]
  #v(0.6em)
  {{/if}}

  // CP explainer (only when content present and modality applies)
  {{#if has_cp_explainer}}
  #block(
    fill: luma(252),
    stroke: (left: 3pt + primary),
    inset: (left: 1em, right: 1em, y: 0.7em),
    radius: (right: 4pt),
    width: 100%
  )[
    #text(size: 10pt, weight: "bold", fill: luma(60))[{{cp_explainer_title}}]
    #v(0.3em)
    #text(size: 9pt)[{{cp_explainer}}]
  ]
  #v(0.6em)
  {{/if}}

  // Substrate oxidation
  {{#if has_substrate_explainer}}
  #block(
    fill: luma(252),
    stroke: (left: 3pt + success),
    inset: (left: 1em, right: 1em, y: 0.7em),
    radius: (right: 4pt),
    width: 100%
  )[
    #text(size: 10pt, weight: "bold", fill: luma(60))[{{substrate_explainer_title}}]
    #v(0.3em)
    {{#if has_substrate_table}}
    #table(
      columns: (1fr, 1fr, 1fr),
      inset: (x: 8pt, y: 5pt),
      stroke: none,
      fill: (col, row) => if row == 0 { luma(235) } else if calc.odd(row) { luma(250) } else { white },
      align: (left, center, center),
      [#text(weight: "bold")[{{stage}}]],
      [#text(weight: "bold")[{{fat_oxidation}}]],
      [#text(weight: "bold")[{{cho_oxidation}}]],
      {{substrate_rows_content}}
    )
    {{else}}
    #text(size: 9pt)[{{substrate_explainer}}]
    {{/if}}
  ]
  {{/if}}
]
#v(0.8em)
{{/if}}

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
