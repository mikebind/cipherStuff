function c19 = makeC19(lett)
% Enciphering alphabets are of this pattern.  Given just the initial letter
% (i.e. what plain 'a' translates to), this constructs the rest of the
% enciphering alphabet. 

alph = 'abcdefghijklmnopqrstuvwxyz';
toLettIdx = @(c) double(lower(c))-96;
c19_idxs = mod(toLettIdx(lett)+19*(0:25),26);
c19_idxs(c19_idxs==0) = 26;
c19 = alph(c19_idxs);
