# GrowthCharts
_interactive tool for visualizing growth curves_

---

`GrowthCharts` Shiny app is an interactive tool to visualize and analyze growth curves of individuals by comparing their growth data to official growth standards. The app takes an input data file containing growth measurements (details in section `Input`) for people of interest and generates personalized growth curves. It calculates the percentiles for each measurement, allowing users to assess how individuals' growth compares to the general population.

The app is particularly useful for parents looking to track and understand growth patterns in their children, monitor development, or assess any deviations from expected standards.

---

__Key Features__

- upload your own measurements
- growth curve visualization
- percentile calculation
- interactive Interface

---

### __Input__

The input data should follow the template structure. The template is located in: (`data/measurement_template.xlsx`).

---

### __Data source__

#### WHO

WHO standard for each feature was downloaded for the two age ranges ([age 0-5](https://www.who.int/tools/child-growth-standards/standards/), [age 5-19](https://www.who.int/tools/growth-reference-data-for-5to19-years/indicators/)), merged, and saved as `.csv` file. 

---

__WHO.Female.BMI.csv__

- [bfa-girls-zscore-expanded-tables.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/body-mass-index-for-age/expanded-tables/bfa-girls-zscore-expanded-tables.xlsx?sfvrsn=ae4cb8d1_12)

- [bmi-girls-z-who-2007-exp.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/bmi-for-age-(5-19-years)/bmi-girls-z-who-2007-exp.xlsx?sfvrsn=79222875_2)

---

__WHO.Female.Head.csv__

- [hcfa-girls-zscore-expanded-tables.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-for-age/expanded-tables/hcfa-girls-zscore-expanded-tables.xlsx?sfvrsn=3a34b8b0_8)

---

__WHO.Female.Height.csv__

- [lhfa-girls-zscore-expanded-tables.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/length-height-for-age/expandable-tables/lhfa-girls-zscore-expanded-tables.xlsx?sfvrsn=27f1e2cb_10)

- [hfa-girls-z-who-2007-exp.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/height-for-age-(5-19-years)/hfa-girls-z-who-2007-exp.xlsx?sfvrsn=79d310ee_2)

---

__WHO.Female.Weight.csv__

- [wfa-girls-zscore-expanded-tables.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/expanded-tables/wfa-girls-zscore-expanded-tables.xlsx?sfvrsn=f01bc813_10)

- [hfa-girls-z-who-2007-exp_7ea58763-36a2-436d-bef0-7fcfbadd2820.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/weight-for-age-(5-10-years)/hfa-girls-z-who-2007-exp_7ea58763-36a2-436d-bef0-7fcfbadd2820.xlsx?sfvrsn=6ede55a4_4)

---

__WHO.Male.BMI.csv__

- [bfa-boys-zscore-expanded-tables.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/body-mass-index-for-age/expanded-tables/bfa-boys-zscore-expanded-tables.xlsx?sfvrsn=f8e1fbe2_10)

- [bmi-boys-z-who-2007-exp.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/bmi-for-age-(5-19-years)/bmi-boys-z-who-2007-exp.xlsx?sfvrsn=a84bca93_2)

---

__WHO.Male.Head.csv__

- [hcfa-boys-zscore-expanded-tables.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/head-circumference-for-age/expanded-tables/hcfa-boys-zscore-expanded-tables.xlsx?sfvrsn=2ab1bec8_8)

---

__WHO.Male.Height.csv__

- [lhfa-boys-zscore-expanded-tables.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/length-height-for-age/expandable-tables/lhfa-boys-zscore-expanded-tables.xlsx?sfvrsn=7b4a3428_12)

- [hfa-boys-z-who-2007-exp.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/height-for-age-(5-19-years)/hfa-boys-z-who-2007-exp.xlsx?sfvrsn=7fa263d_2)

---

__WHO.Male.Weight.csv__

- [wfa-boys-zscore-expanded-tables.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/expanded-tables/wfa-boys-zscore-expanded-tables.xlsx?sfvrsn=65cce121_10)

- [hfa-boys-z-who-2007-exp_0ff9c43c-8cc0-4c23-9fc6-81290675e08b.xlsx](https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/weight-for-age-(5-10-years)/hfa-boys-z-who-2007-exp_0ff9c43c-8cc0-4c23-9fc6-81290675e08b.xlsx?sfvrsn=b3ca0d6f_4)

---

__WHO instructions__

- [Computations](https://cdn.who.int/media/docs/default-source/child-growth/growth-reference-5-19-years/computation.pdf?sfvrsn=c2ff6a95_4)

- [Days to months transformation](https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/instructions-en.pdf?sfvrsn=5cec8c61_23)
