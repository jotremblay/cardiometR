# Plain-language report example (cycling step test)

A two-page French report for participants without a scientific background,
built on `generate_report()` with a custom Typst template.

| File | Purpose |
|------|---------|
| `make_report.R` | Reads a COSMED file, runs the analysis, and renders the PDF. |
| `cpet_report_udem.typ` | Template: title band, four result cards, definitions table, stage table, population comparison, ventilatory thresholds. |

```sh
Rscript make_report.R participant.xlsx
Rscript make_report.R participant.xlsx my_template.typ out.pdf
```

## What the script does beyond `analyze_cpet()`

* **Starting power** is read from the first power of the `exercise` phase
  (right after the warm-up), then `stage_duration` and `increment_size` are
  passed to `analyze_cpet()`.
* **Stage table** is renumbered from 1 (warm-up excluded); full stages show
  their protocol duration and the last stage its real duration.
* **MAP/PPO** (Kuipers) is recomputed from those durations. Recent versions
  of cardiometR do this by themselves (`stage_span_s`); this script also
  works with older ones.
* **Population comparison** is drawn natively in Typst (range bar, mean tick,
  participant marker) from `get_normative_data()`.
* **First ventilatory threshold (SV1)** is reported as "not determined" when
  the first stage already sits above it. The check is a heuristic, not a
  validated method: VE/VO2 at the end of the first stage at least 5 % above
  the end of the warm-up. Review borderline cases by hand.
* The report text uses the preferred French terms of the INS Quebec glossary
  of sport science ("aptitude aerobie", "RER", "premier/deuxieme seuil
  ventilatoire"); the definitions table cites it.

## Things to adapt

* The accent colour (`primary` in the template) is a UdeM-style blue; swap in
  your institution's official value.
* Texts are French and written for cycling; the treadmill cards would need
  their own labels.
* `NUTRITIONAL_STATE` and `CAFFEINE` at the top of the script apply to every
  participant.
