# CPLEX Model Runs

COL D's CPLEX formulation is in `assign_groups_function.R` with some minor modifications.

Use `groups_main.R` to run the model and capture the output for each of the 1,000 hypothetical classes.  The class data is in `/input_data`.

This is all a big hack to be able to actually capture the output from CPLEX that gets dumped to the console.  This was the only way I could figure out to do it without manually recording after every model run.  The objective function can actually be dumped in a much easier way, but this method captures compute time as well (which, unfortunately, happens to not be that interesting).  I will leave it like this because it works.
