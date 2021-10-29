function nsnp_idxs = find_nsnp_from_orig_search(searchString, frame)

locs = findstr(searchString, frame.orig);

nsnp_idxs = frame.nsnp_idxs_in_orig_frame(locs);
