This implementation still runs in O(n^2), since it nondeterministically
tries all possible interpretations of *.  Note that it does not allow
for repeats to be nested, which, to be fair, would be pretty hard to
handle.
