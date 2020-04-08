### Summary

The two projections on the Local and National Health Projections tabs
are adapted from the IHME model and the CHIME model. Each model is
designed with specific assumptions and parameters. The table below
offers a quick comparison of the two models.

    mod <- c("Institution","Modeling Approach","Strengths","Limitations","Location Concentration","Inputs","Outputs","Source")
    ihme <- c("IHME of University of Washington","Data-driven, Statistical","DoD Supported, ICU Capacity","Relies on initial (possibly unreliable) data","U.S. and individual states","Projection timeframe, current hospitalizations, mitigation","Projected cases over time, ICU use","https://covid19.healthdata.org/projections/")
    chime <- c("University of Pennsylvania","SIR-based, deterministic","DoD Suported, ICU Capactiy","Lacks seasonal factors, deterministic","Specific regions with states","Total population, age distribution, epidemiology info, mitigation, severity","Projected cases over time, ICU use","https://penn-chime.phl.io/")
    model.comp <- cbind(mod, ihme, chime)
    colnames(model.comp) <- c("Model","IHME","CHIME")

    #fill in data table code here

### IHME

The IHME model was built by the Institute for Health Metrics and
Evaluation (IHME) at the University of Washington. It assumes that three
of the four mitigation measures of closing schools, closing
non-essential services, shelter-in-place, and significant travel
restricitons have been implemented. The shaded region represents a
region of uncertainty on the projected expected values over time.

### CHIME

The COVID-19 Hospital Impact Model for Epidemics (CHIME) app was created
by the University of Pennsylvania. CHIME provides estimates of daily and
cumulative cases, hospitalizations, ICU admissions, and patients
requiring ventilators. It uses a Susceptible-Infected-Recovered (SIR)
model to generate estimates based on Center for Disease Control and
Prevention (CDC) planning factors. There are many inputs to CHIME
relative to the location of interest. In CHAD, the user has the ability
to change the percent reduction of social contact. Furthermore, the
model in CHAD is an adaptation of CHIME that includes Exposed and
Asympotmatic in addition to Susceptible, Infected, and Asympotmatic.

The shaded region represents a region of uncertainty on the projected
expected values over time. The CHIME model itself does not provide a
region of uncertainty. However, uncertainty was calculated using
CDC-defined best and worst case scenario paramters to generate upper and
lower bound curve. The resulting region of uncertainty often shows a
*crossing* effect. This is because best case and worst case scenarios
move the peak of the curve. When the curve is flattened, the duration of
the pandemic is also lengthened, displaying a *crossing* effect between
the best and worst case projection curves.
