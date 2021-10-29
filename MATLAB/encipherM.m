function cipherText = encipherM(plainText, offsetShiftCycle, offsetMutationShift, initialOffset,plainIdxMultiplier)
if nargin<2 || isempty(offsetShiftCycle)
    offsetShiftCycle = [9, -3, 9, 9, -3, 9, -3, 9, 9, -3, 9, -3, 9]; 
end
cycleLength = length(offsetShiftCycle);
if nargin<3 || isempty(offsetMutationShift)
    offsetMutationShift = 8; 
end
if nargin<4 || isempty(initialOffset)
    initialOffset = 3; 
end
if nargin<5 || isempty(plainIdxMultiplier)
    plainIdxMultiplier = 19;
end

mutationGaps = [161,174];
nMutationGaps = length(mutationGaps);
mutationGapIdx = 1; % start with first gap
initialMutationCountdown = 160; 

% Squeeze the plainText down to a no-space no-punctuation lowercase version
plainTextAsciiCodes = double(plainText);
lower_mask = plainTextAsciiCodes > 96 & plainTextAsciiCodes < 123;
upper_mask = plainTextAsciiCodes > 64 & plainTextAsciiCodes < 91;
nsnp_mask = lower_mask | upper_mask; 
plainText_nsnpl = lower(plainText(nsnp_mask));

toLettIdx = @(c) double(lower(c))-96; 
toLett = @(lettIdx) char(lettIdx+96);
mod26 = @(N) mod(N-1,26)+1; % modulo 26 for 1-based indexing

cipherText_nsnpl = repmat('_', size(plainText_nsnpl));


currentOffset = initialOffset;
nextOffsetShiftIdx = 1;
mutationCountdown = initialMutationCountdown; 
for loc = 1:length(plainText_nsnpl)
    plainLettIdx = toLettIdx(plainText_nsnpl(loc));
    ciphLettIdx = mod26((plainLettIdx - 1)*plainIdxMultiplier + currentOffset); 
    ciphLett = toLett(ciphLettIdx);
    % Add to the cipherText
    cipherText_nsnpl(loc) = ciphLett;
    % Update mutation countdown
    mutationCountdown = mutationCountdown-1;
    if mutationCountdown==0
        % shift mutation cycle index
        nextOffsetShiftIdx = nextOffsetShiftIdx+offsetMutationShift;
        if nextOffsetShiftIdx > cycleLength
            nextOffsetShiftIdx = nextOffsetShiftIdx - cycleLength;
        end
        % Reset mutation countdown
        mutationGapIdx = mutationGapIdx + 1;
        if mutationGapIdx > nMutationGaps
            mutationGapIdx = mutationGapIdx-nMutationGaps;
        end
        mutationCountdown = mutationGaps(mutationGapIdx);        
    end
    % Update currentOffset
    currentOffset = mod26(currentOffset + offsetShiftCycle(nextOffsetShiftIdx));
    % Update nextOffsetShiftIdx
    nextOffsetShiftIdx = nextOffsetShiftIdx +1; 
    if nextOffsetShiftIdx > cycleLength
        nextOffsetShiftIdx = nextOffsetShiftIdx - cycleLength;
    end
end
    
% Fill in and uppercase to match original plaintext
cipherText = plainText;
cipherText(nsnp_mask) = cipherText_nsnpl;
cipherText(upper_mask) = upper(cipherText(upper_mask));
    