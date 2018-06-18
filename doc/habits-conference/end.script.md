Hi, I'm Colin Caine. I joined HABITS about two months ago to help develop a tool to demonstrate how non-specialists could interact with the kind of data collected by GoSmarter and the potential value.

The dream is that this observational tool could be used to drive policy experimentation and data-driven interventions.

Wanted:
 - Freedom to develop our own new features
 - Others don't do it
 - Provide open source base for future analysis tools
   - Flexible on data type
 - OS good for science (transparent, repro, accessibility, cost)

I'm going to show a screenshot of the tool, then go through some of the background of how we went about making it.

---

So, this is what the tool looks like at the moment. That's a map of central Newcastle with lines that represent trips made by some of our study participants.

Lines are coloured by mode with a key in the corner. So far, so similar. The main contribution of this tool is to allow easy exploration of different variables (point) filtered by regions of interest, trip date, time and so on.

Once an interesting subset has been identified you can explore the data in other formats with graphs.

We imagine this tool being used to explore how transport behaviour has changed in response to an intervention, especially very location specific interventions.

---

That's what we've got now, but why did we choose to make that and how did we do it?

The first step is RS, a fancy name for thinking about what people might want and asking them about it.

We wanted stories that other sources don't answer so well, either because they're too expensive or they have bad routing information or they don't capture active modes well.

From that base we came up with about a dozen questions in Leeds which we then discussed with Rob Snowball of Newcastle to see whether real transport planners cared about our questions.

From that process we ended up with nine questions. Of these, we attempt to answer five with this tool and are addressing or planning to address the others differently.

Four of these are related by spatial dependence and that's what I've focussed on.

---

I only joined the project 2 months ago, so tool choice was critical. I needed something that I could make a complete web application in in a few weeks.

And the tool I went with is Shiny.

Shiny is a package for the data science language R that has a well-deserved reputation as a rapid prototyping language for web applications.

The well regarded Propensity to Cycle Tool was prototyped in three months in Shiny.

It's a really fantastic bit of kit tailored for data science problems. It also maintains a split between data held on a remote server and data sent to the user.

- widget library
- conversion between R and javascript
-

ggplot2 is a particularly nice plotting library that I used to parameterise my plotting methods so that different data series can be explored without changing the core code.

R is a high level language that enables the above.

---

So that's gone pretty well, but there have been some problems

Due to data privacy we keep sensitive data in a VRE
 - no internet
 - time lag adding software
 - can't use the tools you want
 - very slow at processing the moderately big data we have

Performance of mapping software is poor with many lines. More computer-sciency approaches are harder in R

---

So I've got a week left, where am I on that original list of requirements?

This tool does support four out of five stories and the code is all ready to go for number 5 and a bunch more. I've finally got the VRE running so
 - health outcome overlays
 - maintentance cost per route
 - journey reason

and more are just around the corner.

---

Demo each bit slowly.

Select a region we care about (St James Park). Journeys in.

Could do this with a transport survey, but it's expensive and you won't get the same density of information

By mode. By METh.
