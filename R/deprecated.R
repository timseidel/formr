
# TODO: 

# The best practice is to turn formr_connect into a soft deprecation wrapper. 
# This wrapper should:
# Warn the user that the function is deprecated.
# Try to map the old arguments to the new system if possible 
# 	(e.g., if they provided credentials that can be swapped).
# Fail gracefully with a clear instruction on what to do next.

# Affected Functions:
# formr_connect()
