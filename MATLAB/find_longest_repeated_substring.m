
function [longestMatchingSubstring,longestStartIdxs] = find_longest_repeated_substring(seq)
% Finds longest non-overlapping substring
% Takes ~1 min for 1e4 characters
% Note that if the string is truly periodic, the returned longest matching
% substring might contain multiple periods.  For example, if the full text
% contained 6 periods, the returned substring would have 3 periods, because
% there is another non-overlapping instance of 3 periods following it. 

seq = seq(:)'; % sequence should be linear
% note that if seq should be read across and then down, it needs to be
% transposed and or linearized before being input to this function, which
% will read down columns by default.

longestRepeatLength=1;

seqLength = length(seq);
seqIdx = 1; 
while seqIdx+2*longestRepeatLength -1 <= seqLength
    toMatch = seq(seqIdx:seqIdx+longestRepeatLength-1);
    matchIdx = seqIdx+longestRepeatLength; 
    while matchIdx + longestRepeatLength -1 <=seqLength
        possibleMatch = seq(matchIdx:matchIdx+longestRepeatLength-1);
        if all(possibleMatch==toMatch)
            % This is now the longest known match
            longestMatchingSubstring = toMatch;
            longestStartIdxs = [seqIdx, matchIdx];
            % See if it can be longer
            while matchIdx+longestRepeatLength<=seqLength...
                    &&  seq(seqIdx+longestRepeatLength)==seq(matchIdx+longestRepeatLength)...
                    && (seqIdx+longestRepeatLength < matchIdx)
                % Next character also matches!
                longestMatchingSubstring = seq(seqIdx:seqIdx+longestRepeatLength);
                toMatch = longestMatchingSubstring;
                longestStartIdxs = [seqIdx, matchIdx];
                longestRepeatLength = longestRepeatLength+1;
            end
        end
        % Try starting at the next matchIdx
        matchIdx = matchIdx+1;
    end
    % Increment seqIdx
    seqIdx = seqIdx+1;
end
