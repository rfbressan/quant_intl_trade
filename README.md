# Quantitative Methods in International Trade

Quantitative Methods in International Trade EESP 2021

Replication files for projects 1 and 2.

## Project 1

The first objective is to implement a multi-sector version of the Eaton and Kortum (2002) model as in Costinot, Donaldson, and Komunjer (2012).

I developed a Shiny webapp hosted in [https://rfbressan.shinyapps.io/trade_project1](https://rfbressan.shinyapps.io/trade_project1). You can simulate 3 scenarios (plus the baseline) and verify the changes implied by the Covid-19 assumptions. The associated report can be download from within the webapp.

## Project 2

Use the algorithm described in Artuç, Chaudhuri, and McLaren (2008) - and used in Artuç, Chaudhuri, and McLaren (2010) - to compute the initial steady state (pre-liberalization), as well as the new steady state and transition paths following a trade liberalization episode. This includes computing wages, labor force allocations and value functions.

The Shiny webapp is available at: [https://rfbressan.shinyapps.io/trade_project2](https://rfbressan.shinyapps.io/trade_project2). Some model parameters can be changed and the model will be re-computed. Time paths for labor force, wage, value function and the treshold value are shown in plots.

Also, there is a Python implementation of the model in the file EconomiaArtuc.py. This contains a [streamlit](https://streamlit.io/) application you can run directly with the command `$ streamlit run EconomicaArtuc.py`, once you have streamlit installed in your environment. It will ask for the time of tariff change (i.e. delayed period) and then will present two figures for the time paths of wages and labor force.
