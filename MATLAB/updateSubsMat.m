function newSubsMat = updateSubsMat(nsnp_idx, plainText, subsMat, frame)

% I want a function which allows me to specify a nsnp_idx, a plaintext
% character string (starting at that index), an existing substitution
% matrix. Result would be an updated substitution matrix, perhaps with some
% info about what was changed...

numLett = length(plainText);
nsnp_idxs = nsnp_idx + (0:(numLett-1));
alph_nums = frame.nsnp_alpha_nums(nsnp_idxs);

newSubsMat = subsMat;
if isempty(newSubsMat)
    newSubsMat = repmat('_',18,26);
end

alphabet = 'abcdefghijklmnopqrstuvwxyz';

for plain_idx = 1:numLett
    plainLett = plainText(plain_idx);
    ciphLett = frame.nsnpl(nsnp_idxs(plain_idx));
    alph_num = alph_nums(plain_idx);
    % alphabet number is in rows, ciphertext letters in columns
    lett_col = findstr(ciphLett,alphabet);
    newSubsMat(alph_num, lett_col) = plainLett;
end