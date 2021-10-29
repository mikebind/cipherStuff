function c11 = makeC11(lett)
% Deciphering alphabets are of this pattern. Given just one initial letter
% (i.e. what cipher 'a' translates to), this constructs the rest of the
% deciphering alphabet.

alph = 'abcdefghijklmnopqrstuvwxyz';
toLettIdx = @(c) double(lower(c))-96;
c11_idxs = mod(toLettIdx(lett)+11*(0:25),26);
c11_idxs(c11_idxs==0) = 26;
c11 = alph(c11_idxs);
