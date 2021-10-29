function plainText = decipher(cipherText, initialDeciph18, firstMutationLoc)


% Squeeze the cipher text down to just decipherable characters (no spaces,
% no punctuation, lowercase)
cipherTextAsciiCodes = double(cipherText);
lower_mask = cipherTextAsciiCodes > 96 & cipherTextAsciiCodes < 123;
upper_mask = cipherTextAsciiCodes > 64 & cipherTextAsciiCodes < 91;
nsnp_mask = lower_mask | upper_mask; 
cipherText_nsnpl = lower(cipherText(nsnp_mask));

% Set default first mutation at loc 5
if nargin<3 || isempty(firstMutationLoc)
    firstMutationLoc=5;
end
% Set default initial set of encipherment alphabet starters
if nargin<2 || isempty(initialDeciph18)
    initialDeciph18 = 'ejqvchmtydkpwbgnsz';
end

firstLongGapIsMutation = 11; % this mutation number is the first long one

numLettsBetweenMutationsShort = 13; % mut, then 12 non-mut, then mut
numLettsBetweenMutationsLong = numLettsBetweenMutationsShort+18; % mut, then 30 non-mut, then mut

mutationShift = -2;
mutationCountdown = firstMutationLoc-1; % mutation occurs when it hits zero
longGapCountdown = firstLongGapIsMutation;
longGapCountdownWas = 11; % First 11, then 12

toLettIdx = @(c) double(lower(c))-96; 
toLett = @(lettIdx) char(lettIdx+96);
plusMutShift = @(lett) char(toLett(mod(toLettIdx(lett)+mutationShift-1,26)+1)); 

currentDeciphMatrix = repmat('_',18,26);
for alphNum=1:18
    currentDeciphMatrix(alphNum,:) = makeC11(initialDeciph18(alphNum));
end

plainText_nsnpl = repmat('_', size(cipherText_nsnpl));

for loc = 1:length(cipherText_nsnpl)
    alphNum = mod(loc-1, 18)+1; % Which of the current decipherment alphabets should be used
    % Are we due for a mutation?
    if mutationCountdown==0
        % Apply mutation at this location
        % by shifting forward mutationShift letters
        %fprintf('%i\n',loc);
        curStartLett = currentDeciphMatrix(alphNum,1);
        newStartLett = plusMutShift(curStartLett);
        currentDeciphMatrix(alphNum,:) = makeC11(newStartLett);
        % Reset the countdown to the next mutation
        if longGapCountdown==0
            mutationCountdown = numLettsBetweenMutationsLong;
            if longGapCountdownWas==11
                longGapCountdown=12;
                longGapCountdownWas=12;
            else 
                longGapCountdown=11;
                longGapCountdownWas=11;
            end
        else
            mutationCountdown = numLettsBetweenMutationsShort;
        end
        % Decrement longGapCountdown because we just did a mutation
        longGapCountdown = longGapCountdown-1;    
    end
    % Decipher this letter
    ciphLett = cipherText_nsnpl(loc);
    plainLett = currentDeciphMatrix(alphNum, toLettIdx(ciphLett));
    % Decrement the countdown to the next mutation
    mutationCountdown = mutationCountdown-1;
    % Add to the cipherText
    plainText_nsnpl(loc) = plainLett;
end
    
% Fill in and uppercase to match original plaintext
plainText = cipherText;
plainText(nsnp_mask) = plainText_nsnpl;
plainText(upper_mask) = upper(plainText(upper_mask));