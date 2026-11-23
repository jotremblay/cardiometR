// CPET Report Template for cardiometR
// Bilingual support (English/French)
// Clinical layout: numbered sections, hairline rules, no decorative fills.

#set document(
  title: "{{title}}",
  author: "{{institution}}"
)

// Palette. Ink and rules are neutral; the accent carries the section
// numbering and the values that matter.
#let primary = rgb("#0057AC")
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
#let tint = rgb("#eef4fb")
#let tint_row = rgb("#f4f8fc")
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
          text(size: 7pt, fill: faint)[Rapport EECR · {{test_date}}]
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
      align(left)[{{patient_name}} · {{test_date}}],
      align(center)[#context counter(page).display("1 / 1", both: true)],
      align(right)[_cardiometR_]
    )
  ]
)

#set text(
  font: ("Avenir Next", "Helvetica Neue", "Arial", "sans-serif"),
  size: 9.5pt,
  fill: ink
)
#set par(leading: 0.55em, justify: false)

// Readable subscripts/superscripts (default 0.6em is too small in small text)
#set sub(size: 0.8em)
#set super(size: 0.8em)

// Package-generated tables (stage table) follow the same look.
#set table(
  fill: (x, y) => if y == 0 { primary } else if calc.even(y) { tint_row } else { none },
  stroke: (x, y) => (bottom: if y == 0 { none } else { 0.4pt + rule_hair }),
  inset: (x: 3mm, y: 2mm),
)
#show table.cell.where(y: 0): set text(fill: white, weight: 700)

#show "QR": [RER]

#show raw: set text(font: ("SF Mono", "Menlo", "Monaco", "monospace"))

// Numbered section rule. The counter keeps the numbers contiguous even
// when the operator switches a section off.
#let seccount = counter("cardiometr-section")
#let sec(title) = block(above: 5mm, below: 2mm, width: 100%)[
  #seccount.step()
  #grid(
    columns: (auto, auto, 1fr),
    column-gutter: 2.5mm,
    align: horizon,
    box(fill: primary, radius: 1mm, inset: (x: 1.6mm, y: 0.9mm), text(size: 8pt, weight: 700, fill: white)[#context {
      let n = seccount.get().first()
      if n < 10 [0#n] else [#n]
    }]),
    text(size: 11pt, weight: 700, tracking: 0.8pt, fill: primary)[#upper(title)],
    line(length: 100%, stroke: 0.8pt + rule_strong)
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
  inset: (x: 3mm, y: 2mm),
  fill: (x, y) => if y == 0 { primary } else if calc.even(y) { tint_row } else { none },
  stroke: (x, y) => (bottom: if y == 0 { none } else { 0.4pt + rule_hair }),
  ..cells
)

#let th(body) = text(size: 7.5pt, weight: 700, tracking: 0.6pt, fill: white)[#upper(body)]


// ══════ Title block ══════

#block(above: 0mm, below: 3mm, width: 100%, fill: primary, radius: 1.5mm, inset: (x: 6mm, y: 4.5mm))[
  #grid(
    columns: (1fr, auto),
    align: bottom,
    text(size: 7.5pt, weight: 700, tracking: 1.5pt, fill: white.transparentize(15%))[#upper[{{report_kicker}}]],
    text(size: 8.5pt, fill: white.transparentize(15%))[{{test_date}}]
  )
  #v(1.5mm)
  #text(size: 24pt, weight: 800, fill: white)[{{patient_name}}]
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
        ..kv([{{label_medications}}], [{{medication_list}}]),
        {{#if caffeine_intake}}
        ..kv([{{label_caffeine}}], [Oui]),
        {{else}}
        ..kv([{{label_caffeine}}], [Non]),
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
    columns: (1fr, 1fr, 1fr, 1fr),
    column-gutter: 3mm,
    block(width: 100%, fill: tint, radius: 1.5mm, stroke: (top: 2.5pt + primary), inset: (x: 3.5mm, y: 3.5mm))[
      #stack(dir: ttb, spacing: 2.6mm,
        text(size: 7pt, weight: 700, tracking: 0.4pt, fill: muted)[#upper[Aptitude aérobie]],
        text(size: 22pt, weight: 800, fill: primary)[{{ap_card1_value}}],
        text(size: 8.5pt, fill: muted)[{{ap_card1_unit}}],
      )
    ],
    block(width: 100%, fill: tint, radius: 1.5mm, stroke: (top: 2.5pt + primary), inset: (x: 3.5mm, y: 3.5mm))[
      #stack(dir: ttb, spacing: 2.6mm,
        text(size: 7pt, weight: 700, tracking: 0.4pt, fill: muted)[#upper[Puissance max. relative]],
        text(size: 22pt, weight: 800, fill: primary)[{{ap_card2_value}}],
        text(size: 8.5pt, fill: muted)[{{ap_card2_unit}}],
      )
    ],
    block(width: 100%, fill: tint, radius: 1.5mm, stroke: (top: 2.5pt + primary), inset: (x: 3.5mm, y: 3.5mm))[
      #stack(dir: ttb, spacing: 2.6mm,
        text(size: 7pt, weight: 700, tracking: 0.4pt, fill: muted)[#upper[Puissance maximale]],
        text(size: 22pt, weight: 800, fill: primary)[{{ap_card3_value}}],
        text(size: 8.5pt, fill: muted)[W],
      )
    ],
    {{#if has_ftp_block}}
    block(width: 100%, fill: tint, radius: 1.5mm, stroke: (top: 2.5pt + primary), inset: (x: 3.5mm, y: 3.5mm))[
      #stack(dir: ttb, spacing: 2.6mm,
        text(size: 7pt, weight: 700, tracking: 0.4pt, fill: muted)[#upper[Estimation FTP]],
        text(size: 22pt, weight: 800, fill: primary)[{{ftp_low}}–{{ftp_high}}],
        text(size: 8.5pt, fill: muted)[W],
      )
    ],
    {{/if}}
  )
]
{{/if}}

// ══════ Définitions ══════

#block(breakable: false, width: 100%)[
  #sec[Définitions]
  #clinical_table(
    columns: (38mm, 1fr),
    align_spec: (left + top, left + top),
    th[Terme], th[Définition],
    text(weight: 600)[Aptitude aérobie (VO#sub[2]max / VO#sub[2]pic)],
    [Quantité maximale d'oxygène que le corps peut absorber et utiliser par minute lors d'un effort intense, exprimée par kilo de poids corporel (mL/kg/min). Plus la valeur est élevée, meilleure est la capacité du cœur, des poumons et des muscles à produire de l'énergie à l'effort.
      #v(1mm)
      *VO#sub[2]max :* valeur atteinte lorsque la consommation d'oxygène cesse d'augmenter malgré l'augmentation de l'effort (plateau). Elle confirme que le participant a atteint son maximum.
      #v(1mm)
      *VO#sub[2]pic :* valeur la plus élevée mesurée pendant le test lorsque ce plateau n'est pas observé. Elle peut être légèrement inférieure au véritable maximum.],
    text(weight: 600)[Puissance maximale relative],
    [Puissance maximale atteinte pendant le test, calculée par kilo de poids corporel (W/kg). Elle permet de comparer des personnes de poids différents.],
    text(weight: 600)[VE (ventilation)],
    [Quantité d'air respiré en une minute, exprimée en litres par minute (L/min). Plus l'effort est intense, plus la respiration s'accélère et s'approfondit, et plus la VE augmente.],
    text(weight: 600)[RER (quotient d'échange respiratoire)],
    [Rapport entre le gaz carbonique rejeté et l'oxygène consommé, mesuré à la bouche. Il indique le type de carburant utilisé par le corps : autour de 0,7, surtout des graisses ; autour de 1,0, surtout des glucides. À l'effort maximal, il dépasse généralement 1,10, ce qui montre que le participant s'est rendu à sa limite.],
    text(weight: 600, fill: vt1_col)[Premier seuil ventilatoire (SV1)],
    [Premier point où la respiration s'accélère plus vite que l'effort : la ventilation augmente de façon disproportionnée par rapport à l'oxygène consommé, mais reste proportionnelle au gaz carbonique produit. En dessous, l'effort est confortable et peut être maintenu longtemps.],
    text(weight: 600, fill: vt2_col)[Deuxième seuil ventilatoire (SV2)],
    [Deuxième point où la respiration s'emballe : la ventilation augmente de façon disproportionnée par rapport au gaz carbonique produit. Au-delà, l'effort devient très difficile et ne peut être maintenu que peu de temps.],
  )
  #v(1mm)
  #caption[Définitions adaptées du glossaire de Thibault, _Les mots des sciences du sport en français_, Institut national du sport du Québec.]
]

#pagebreak()

// ══════ 07 Stage-by-stage results ══════

{{#if has_stage_table}}
#sec[{{section_stage_table}}]
{{stage_table}}
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
  %%COMPARAISON%%
]
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
    %%VT1ROW%%
    %%VT2ROW%%
  )
]
{{/if}}

// ══════ 10 Nine-panel display ══════

{{#if graphs_missing}}
#block(
  width: 100%,
  fill: rgb("#fff3cd"),
  stroke: 1pt + rgb("#856404"),
  inset: 3mm,
  radius: 2pt
)[
  #text(size: 9pt, weight: "bold", fill: rgb("#856404"))[{{graphs_missing_banner}}]
]
#v(2mm)
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
