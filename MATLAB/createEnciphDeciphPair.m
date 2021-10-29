function [enciphFcn, deciphFcn] = createEnciphDeciphPair(multiplier,...
    initialOffset, offsetShiftCycle, mutationCycleShift, mutationGaps,...
    initialMutationCountdown)

% Establish defaults
if nargin<1 || isempty(multiplier)
    multiplier = 19; 
end
if nargin<2 || isempty(initialOffset)
    initialOffset = 3; 
end
if nargin<3 || isempty(offsetShiftCycle)
    offsetShiftCycle = [9, -3, 9, 9, -3, 9, -3, 9, 9, -3, 9, -3, 9];
end
if nargin<4 || isempty(mutationCycleShift)
    mutationCycleShift = 8; 
end
if nargin<5 || isempty(mutationGaps)
    mutationGaps = [161, 174];
end
if nargin<6 || isempty(initialMutationCountdown)
    initialMutationCountdown = 160;
end

mod26 = @(N) mod(N-1,26)+1;

% Input validation
multiplier = mod26(multiplier);
allowedMultipliers = [1,3,5,7,9,11,15,17,19,21,23,25]; 
% Note that 1 means no encipherment (plain==ciph), and 25 means a reversed
% order normal alphabet (a=z, b=y, c=x, etc). Multipliers > 26 are allowed,
% but behave the same as taking mod26 of them first. For example, a
% multiplier of 29 is the same as a multiplier of 3 (because 29 * N mod 26 
% is the same as 3 * N mod 26 (because (26 + 3) * N = 26*N + 3*N and 26*N
% mod 26 is zero.  
if ~any(multiplier==allowedMultipliers)
    error('Multiplier must not share any factors with 26!');
end

% Helper anonymous functions
findPairedMultiplier = @(multiplier) find(mod26([1:26]*multiplier)==1);

% How are initial offsets related?
% outputLettIdx = (inputLettIdx-1)*mult1 + offset1;
% inputLettIdx = (outputLettIdx-1)*mult2 + offset2;
% Note that these relationships must hold for any inputLettIdx, so for
% convenience, let's choose inputLettIdx=1.  In this case, outputLettIdx =
% offset1 because the multiplier drops out (multiplied by 1-1 = 0).
% Therefore plugging into the second relationship:
% 1 = (offset1-1)*mult2 + offset2
% Solving for offset2
% offset2 = 1 - mult2*(offset1-1)
% Everything is mod26 of course...
findPairedOffset = @(multiplier1, offset1) mod26(1-findPairedMultiplier(multiplier1)*(offset1-1));

% How are the offset shifts related?
% Though some algebra, this can be simplified down to 
findPairedOffsetShiftCycle = @(multiplier1, offsetShiftCycle1) mod26(findPairedMultiplier(multiplier1) .* (-offsetShiftCycle1));

enciphFcn = @(plainText) cipherFcn(plainText, multiplier,...
    initialOffset, offsetShiftCycle, mutationCycleShift, mutationGaps,...
    initialMutationCountdown);

pairedMultiplier = findPairedMultiplier(multiplier);
pairedInitialOffset = findPairedOffset(multiplier, initialOffset);
pairedOffsetShiftCycle = findPairedOffsetShiftCycle(multiplier, offsetShiftCycle);

deciphFcn = @(cipherText) cipherFcn(cipherText, pairedMultiplier,...
    pairedInitialOffset, pairedOffsetShiftCycle, mutationCycleShift, mutationGaps,...
    initialMutationCountdown);
end

function convertedText = cipherFcn(textToConvert, multiplier,...
    initialOffset, offsetShiftCycle, mutationCycleShift, mutationGaps,...
    initialMutationCountdown)
% Parameterized Encipher/Decipher converter function

% Default parameter values if not supplied (matches original encipherment
% scheme)
if nargin<2 || isempty(multiplier)
    multiplier = 19;
end
if nargin<3 || isempty(initialOffset)
    initialOffset = 3; 
end
if nargin<4 || isempty(offsetShiftCycle)
    offsetShiftCycle = [9, -3, 9, 9, -3, 9, -3, 9, 9, -3, 9, -3, 9]; 
end
cycleLength = length(offsetShiftCycle);
if nargin<5 || isempty(mutationCycleShift)
    mutationCycleShift = 8; 
end
if nargin<6 || isempty(mutationGaps)
    mutationGaps = [161, 174];
end
if nargin<7 || isempty(initialMutationCountdown)
    initialMutationCountdown = 160;
end


nMutationGaps = length(mutationGaps);
mutationGapIdx = 1; % start with first gap

% Squeeze the textToConvert down to a no-space no-punctuation lowercase version
textToConvertAsciiCodes = double(textToConvert);
lower_mask = textToConvertAsciiCodes > 96 & textToConvertAsciiCodes < 123;
upper_mask = textToConvertAsciiCodes > 64 & textToConvertAsciiCodes < 91;
nsnp_mask = lower_mask | upper_mask; 
nsnpl = lower(textToConvert(nsnp_mask));

toLettIdx = @(c) double(lower(c))-96; 
toLett = @(lettIdx) char(lettIdx+96);
mod26 = @(N) mod(N-1,26)+1; % modulo 26 for 1-based indexing
modCycleLength = @(N) mod(N-1, cycleLength)+1;

converted_nsnpl = repmat('_', size(nsnpl));

currentOffset = initialOffset;
nextOffsetShiftIdx = 1;
mutationCountdown = initialMutationCountdown; 
for loc = 1:length(nsnpl)
    toConvertLettIdx = toLettIdx(nsnpl(loc));
    convertedLettIdx = mod26((toConvertLettIdx - 1)*multiplier + currentOffset); 
    convertedLett = toLett(convertedLettIdx);
    % Add to the cipherText
    converted_nsnpl(loc) = convertedLett;
    % Update mutation countdown
    mutationCountdown = mutationCountdown-1;
    if mutationCountdown==0
        % shift mutation cycle index
        nextOffsetShiftIdx = modCycleLength(nextOffsetShiftIdx+mutationCycleShift);
        
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
    nextOffsetShiftIdx = modCycleLength( nextOffsetShiftIdx +1); 
   
end
    
% Fill in and uppercase to match original plaintext
convertedText = textToConvert; % copies all text (so that spaces, punctuation, line breaks, which are not converted, are transferred)
convertedText(nsnp_mask) = converted_nsnpl; % replace all converted text
convertedText(upper_mask) = upper(convertedText(upper_mask)); % restore capitalization pattern
end