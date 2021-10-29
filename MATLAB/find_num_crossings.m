function N = find_num_crossings(start,fin, len, bound_vect)

N = sum( bound_vect>=(start+len) & bound_vect<=fin);