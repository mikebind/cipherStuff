function alphNums = findAlphNumsFromStartLett(startLett, ciph_alph_starts)

%[r,~] = find(startLett==ciph_alph_starts);
%alphNums = r;

% I want a vectorized version for multiple startLetters
alphNums = zeros(2,length(startLett));

for row = 1:size(ciph_alph_starts,1)
    for col = 1:size(ciph_alph_starts,2)
        start = ciph_alph_starts(row,col);
        alphNums(col, startLett==start) = row;
    end
end