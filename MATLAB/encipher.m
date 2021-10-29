function cipherText = encipher(plainText, initialEnciph18, firstMutationLoc) 
%

% Squeeze the plainText down to a no-space no-punctuation lowercase version
plainTextAsciiCodes = double(plainText);
lower_mask = plainTextAsciiCodes > 96 & plainTextAsciiCodes < 123;
upper_mask = plainTextAsciiCodes > 64 & plainTextAsciiCodes < 91;
nsnp_mask = lower_mask | upper_mask; 
plainText_nsnpl = lower(plainText(nsnp_mask));

% Set default first mutation at loc 5
if nargin<3 || isempty(firstMutationLoc)
    firstMutationLoc=5;
end
% Set default initial set of encipherment alphabet starters
if nargin<2 || isempty(initialEnciph18)
    initialEnciph18 = 'cliroxgdmvsbyhqnwt';
end

firstLongGapIsMutation = 11; % this mutation number is the first long one

numLettsBetweenMutationsShort = 13; % mut, then 12 non-mut, then mut
numLettsBetweenMutationsLong = numLettsBetweenMutationsShort+18; % mut, then 30 non-mut, then mut

mutationShift = 12;
mutationCountdown = firstMutationLoc-1; % mutation occurs when it hits zero
longGapCountdown = firstLongGapIsMutation;
longGapCountdownWas = 11; % First 11, then 12

toLettIdx = @(c) double(lower(c))-96; 
toLett = @(lettIdx) char(lettIdx+96);
plusMutShift = @(lett) char(toLett(mod(toLettIdx(lett)+mutationShift-1,26)+1)); 

currentEnciphMatrix = repmat('_',18,26);
for alphNum=1:18
    currentEnciphMatrix(alphNum,:) = makeC19(initialEnciph18(alphNum));
end

cipherText_nsnpl = repmat('_', size(plainText_nsnpl));

for loc = 1:length(plainText_nsnpl)
    alphNum = mod(loc-1, 18)+1; % Which of the current encipherment alphabets should be used
    % Are we due for a mutation?
    if mutationCountdown==0
        % Apply mutation at this location
        % by shifting forward mutationShift letters
        %fprintf('%i\n',loc);
        curStartLett = currentEnciphMatrix(alphNum,1);
        newStartLett = plusMutShift(curStartLett);
        currentEnciphMatrix(alphNum,:) = makeC19(newStartLett);
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
    % Encipher this letter
    plainLett = plainText_nsnpl(loc);
    ciphLett = currentEnciphMatrix(alphNum, toLettIdx(plainLett));
    % Decrement the countdown to the next mutation
    mutationCountdown = mutationCountdown-1;
    % Add to the cipherText
    cipherText_nsnpl(loc) = ciphLett;
end
    
% Fill in and uppercase to match original plaintext
cipherText = plainText;
cipherText(nsnp_mask) = cipherText_nsnpl;
cipherText(upper_mask) = upper(cipherText(upper_mask));