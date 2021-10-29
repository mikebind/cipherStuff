function matches = find_repeated_char_seqs(s, seq_length)

matches = {};
for start = 1:length(s)-seq_length
    str_to_match = s(start:start+seq_length-1);
    for idx = start+1:length(s)-seq_length
        if s(idx:idx+seq_length-1)==str_to_match
            matches{end+1} = {str_to_match, start, idx, mod(idx-start,18)};
        end
    end
end
            