function matches = find_repeated_words(o_)
% Given a character vector where non-word characters are represented by
% underscores, this function finds all repeated words.
% It outputs a cell array with one cell per repeated word; within
% that cell the first cell is the word and the second cell is a vector
% of start positions where that word occurs in the input. The number of 
% repetitions can be inferred from the length of the vector.

matches = {};
underscores = o_=='_';
nonunderscores = ~underscores;
starts = find(nonunderscores(2:end) & underscores(1:end-1)) + 1;
starts = [1, starts];
ends = find(underscores(2:end) & nonunderscores(1:end-1));
words = cell(length(starts),1);
for starts_idx = 1:length(starts)
    start = starts(starts_idx);
    fin = ends(starts_idx);
    words{starts_idx} = o_(start:fin);
end
uniq = unique(words);

for uniq_idx = 1:length(uniq)
    word = uniq{uniq_idx};
    match_words_idxs = find(strcmp(word, words));
    if length(match_words_idxs)>1
        % This word appears multiple times
        num_appear = length(match_words_idxs);
        o_idxs = starts(match_words_idxs);
        matches{end+1} = {word, o_idxs};
    end
end

