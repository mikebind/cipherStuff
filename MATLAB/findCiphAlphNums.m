function first_gold_lett = findCiphAlphNums(gold,ciph)
% Derive the starting letter of the c11 which places gold letter "gold" at
% the place corresponding to cipher letter ciph

alph = 'abcdefghijklmnopqrstuvwxyz';

% fcn to convert letter to place in alphabet (a to 1 , b to 2, etc)
toLettIdx = @(c) double(lower(c))-96;
ciph_idx = toLettIdx(ciph);
gold_idx = toLettIdx(gold);

% For example, if I know gold 'g' is at ciph_idx 10, then what gold letter is at
% ciph_idx 0? Well, since I know each letter is 11 later than the previous
% one, I can just go back 9 steps of 11 each. 
first_gold_lett_idx = mod(gold_idx-(ciph_idx-1)*11,26);
first_gold_lett_idx(first_gold_lett_idx==0) = 26;
first_gold_lett = alph(first_gold_lett_idx);
