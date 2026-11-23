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
* **Ventilatory thresholds** are reported as "not determined" when they are
  not reliable. This is a heuristic, not a validated method: the SV1 is
  dropped when VE/VO2 at the end of the first stage is at least 5 % above the
  end of the warm-up, or when it sits at or above the lower bound of the
  estimated FTP (0.72 x MAP), since the SV1 should lie below the FTP. The
  SV2 is dropped when it is missing or not above the SV1. A test that starts
  at about 70 % of VO2peak often leaves no stage below the SV1. Review these
  cases by hand.
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
