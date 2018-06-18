---
author:
 - Colin Caine
 - Tom Redfern
institute: University of Leeds
title: HABITS Decision Support Tool
theme: metropolis
themeoptions:
 - "titleformat=allsmallcaps"
 - "progressbar=frametitle"
header-includes:
 - '\def\tightlist{}'
 - '\newcommand{\light}[1]{\textcolor{lightgray}{#1}}'
 - '\usepackage[export]{adjustbox}'
...

# Why?

- Explore how non-specialists could benefit from these data
- Open source

# What?

![](end_screen.png)

# How?

- Data from *GoSmarter* this time
- Requirements solicitation
- Tool choice
- Tool development

# Requirements solicitation

<div class=notes>
fancy name for "ask people what they want"
</div>

- Started with some "user stories" from discussion within HABITS team
- Refined and modified by Newcastle City Council

# Requirements solicitation

 1. What proportion of journeys/journey km were made by each mode over time in some area?
 1. How do personal outcomes change by IMD over time?
 1. Do people living near this train station cycle to local places and drive to the city centre?
 1. Has travel on this route increased relative to other routes in the region?
 1. What is the demand model for a locality (especially active modes)?

 1. \light{What kind of people respond best to intervention X?}
 1. \light{Have in-app incentives increased use of public transport?}
 1. \light{What is the decay rate of the effectiveness of an intervention?}
 1. \light{Can we assess the value for money of an existing or in-progress scheme?}

# Requirements solicitation

Turns out space is important to transport

 1. What proportion of journeys/journey km were made by each mode over time in some area?
 1. How do personal outcomes change by IMD over time?
 1. Do people living near this train station cycle to local places and drive to the city centre?
 1. Has travel on this route increased relative to other routes in the region?
 1. What is the demand model for a locality (especially active modes)?

# Tool choice

\begin{center}
    \includegraphics[height=30mm]{shiny-logo.png}
    \includegraphics[height=30mm]{rstudio-logo.png}
    \includegraphics[height=30mm]{ggplot2-logo.png}
\end{center}

 - \alert{Shiny} is a fantastic bit of software for prototyping
 - \alert{ggplot2} is a lovely plotting library
 - \alert{R Studio} and \alert{R} glue it together

# Technical limitations

 - Data is sensitive
      - Safe virtual research environment is slow
      - Friction adding and removing

 - Big enough data for performance problems
      - pct.bike

# Achieved

\alert{Yes!}

 1. What proportion of journeys/journey km were made by each mode over time in some area?
 1. Do people living near this train station cycle to local places and drive to the city centre?
 1. Has travel on this route increased relative to other routes in the region?
 1. What is the demand model for a locality (especially active modes)?

Not yet

 5. How do personal outcomes change by IMD over time?

# Other contributions

 - Improvements to libraries used (leaflet, sf, mapedit)
 - Productivity tools for VRE

# Demo

![](end_screen.png)

# Me

Colin Caine

cmcaine@gmail.com
