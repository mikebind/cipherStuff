function plainLett = ciph2plain(ciphLett, deciphAlphStart)
% deciphAlphStart is also known as firstGoldLett
toLettIdx = @(c) double(lower(c))-96; 
% Deciph alphabets are makeC11
deciphAlph = makeC11(deciphAlphStart);
plainLett = deciphAlph(toLettIdx(ciphLett));