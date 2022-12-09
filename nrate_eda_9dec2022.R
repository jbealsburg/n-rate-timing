# Targeted hypothesis testing of the N-rate timing dataset
# Jesse
# 9Dec2022
# Due date: 12Dec2022


# import data -------------------------------------------------------------



# Does iwg grain yield differ by timing? ----------------------------------

# Ho: iwg grain yield does not differ when fertilizer is applied at different
# times



# yld~timing*age*site

# Applied research question addressed: "I'd prefer to fertilize in fall vs.
# spring, can I apply fertilize in the fall and get high yields?


# Does iwg grain yield differ by rate? ------------------------------------

# Ho: Grain yield does not differ among different fertilizer rates


# yld~rate*age*site subsetted within timing?
# yld~timing*rate*age*site

# Applied research question addressed: "I'd prefer to fertilize in fall vs.
# spring, can I apply fertilize in the fall and get high yields? Do I need to
# apply more fertilizer in the fall to achieve the same yield response as a
# spring application?



# IF iwg grain yields differ by N rate, how do we model? ------------------


# 4 models: no effect, linear, quadratic, quadratic planar


# What is the optimum N rate for grain yield? -----------------------------



# Which treatment does best? ----------------------------------------------

# Assuming you have to pick a fertilizer program to do every year, what is the
# best program?

# yld~treatment*site*year


# Do grain yields differ when fertilizer is split applied? ----------------




