function b = findb(a, ma, mb)
% Find the integer or integers b which make ma*a+mb*b a multiple of 26. 
% a, b, ma, and mb should all be integers between 1 and 26

% maxLim should be set such that 
% b < 27
maxLim=26;
tmp = (26*(1:maxLim) - ma*a)./mb;
mask = tmp==round(tmp);
b_cand = tmp(mask);
b = b_cand(b_cand<=26 & b_cand>0);

