function [frame,c] = build_oriented_frame(ospl, step_boundaries, numAlphas)
% ospl = original ciphertext with spaces, punctuation, line breaks, and
% capitalization

if nargin<3 || isempty(numAlphas)
    numAlphas = 18;
end

%% Build no-space no-punct no-numbers vector
ospl_as_integers = cast(ospl, 'uint8');
% newlines are 10, for future reference
% Ciphertext lowercase letters are 97-122, uppercase letters are 65-90
lower_mask = ospl_as_integers > 96 & ospl_as_integers < 123;
upper_mask = ospl_as_integers > 64 & ospl_as_integers < 91;
nsnp_mask = lower_mask | upper_mask; 
orig_idxs = 1:length(ospl);
orig_idxs_in_nsnp_frame = orig_idxs(nsnp_mask); % holds the indices of nsnp characters in the original text
nsnp = ospl(orig_idxs_in_nsnp_frame);
nsnpl = lower(nsnp); % all lowercase version

nsnp_idxs_in_orig_frame = zeros(size(ospl));
nsnp_idxs_in_orig_frame(nsnp_mask) = 1:length(nsnp);


% Here, we just need the nsnpl vector and the vector of step boundaries.
% We then can go from the end of the nsnpl vector back towards the start,
% inserting 14 spaces just before each step boundary location. 
% Also need to keep track of nsnpl indices into ori as it is built (or
% after if that seems easier, which it does).
oriv = nsnpl;
%fourteen_spaces = repmat(' ',[1,14]);
four_spaces = repmat(' ',[1,4]); % actually, I had the sign wrong on this, needed +4 spaces, not -4
nStepBoundaries = length(step_boundaries);
for bnd_idx = 1:nStepBoundaries
    bnd = step_boundaries(nStepBoundaries - bnd_idx+1); % REVERSE order (so that we don't have to track the shifts)
    oriv = [oriv(1:bnd-1), four_spaces, oriv(bnd:end)];
end
oriv_idxs_in_nsnp_frame =  find(oriv~=' ');
% How many spaces are needed to pad oriv to be able to reshape into Nx18
padLength = ceil(length(oriv)/numAlphas)*numAlphas - length(oriv); % 3, same padding needed for nsnp wrapping
pad_spaces = repmat(' ', [1,padLength]);
oriv = [oriv,pad_spaces];
% For a character whose nsnp_idx = 10, that character's location in the ori
% vector is ori_idxs_in_nsnp_frame(10).
% To go the other way (i.e. to find the nsnp index of a character at ori
% vector location ori_idx), we want nsnp_idxs_in_oriv_frame(ori_idx).
% Non-nsnp locations can have zero for nsnp index
nsnp_idxs_in_oriv_frame = zeros(size(oriv));
nsnp_idxs_in_oriv_frame(oriv~=' ') = 1:length(nsnpl);

% Wrap to Nx18, but want to go across rows instead of down columns, do this
% by doing down columns to make 18xN, then transpose.
ori = reshape(oriv,numAlphas,[])';
nsnp_idxs_in_ori_frame = reshape(nsnp_idxs_in_oriv_frame, numAlphas,[])';

% To build ori_idxs_in_nsnp_frame(nsnp_idx), we have to take into account
% the reordering caused by the transpose. This is trickier than before
% because the ordering is no longer monotonic. 
% This is the correspondence between the two
cor = [nsnp_idxs_in_ori_frame(:), (1:length(ori(:)))'];
% Remove places where nsnp_idx is zero
zero_mask = cor(:,1)==0;
cor(zero_mask,:) = [];
% Sort the rows by the nsnp_idx
sort_cor = sortrows(cor,1);
%ori_idxs_in_nsnp_frame(nsnp_idx) = ori_idx; 
ori_idxs_in_nsnp_frame = sort_cor(:,2)';

%% Linking ori frame back to original frame...
%orig_idx_in_ori_frame(ori_idx) = orig_idx
% Bridge back through nsnp idx
orig_idxs_in_ori_frame = zeros(size(ori));
valid_mask = nsnp_idxs_in_ori_frame>0; % has zeros where no nsnp letter
orig_idxs_in_ori_frame(valid_mask) = orig_idxs_in_nsnp_frame(nsnp_idxs_in_ori_frame(valid_mask));

% The reverse is also tricky, because there are gaps in both
% representations now. 
% I just need to replace the nsnp_idx with the corresponding ori idx
ori_idxs_in_orig_frame = zeros(size(ospl));
valid_mask = nsnp_idxs_in_orig_frame>0; 
ori_idxs_in_orig_frame(valid_mask) = ori_idxs_in_nsnp_frame;

% Round-trip works
% ospl(orig_idx) == ori(ori_idxs_in_orig_frame(orig_idx))
% return true as long as orig_idx represents a letter location in the orig
% text (ospl).

%% Alphabet Numbers?
% Derive an alphabet number for each nsnp index, and then for each orig
% location (with 0 for non-letters).
% The alphabet number is just the column number for the letter in the ori
% frame, therefore it can be derived from the ori_idx and the size of ori
% (using ind2sub for column extraction or doing a little math). 
ori_alpha_nums = repmat(1:numAlphas,size(ori,1),1);
nsnp_alpha_nums = ori_alpha_nums(ori_idxs_in_nsnp_frame);
orig_alpha_nums = zeros(size(ospl));
orig_alpha_nums(nsnp_idxs_in_orig_frame>0) = nsnp_alpha_nums; % this is OK because nsnp ordering in orig_frame is monotonic


%% Converters
orig2nsnp = @(orig_idx) nsnp_idxs_in_orig_frame(orig_idx);
nsnp2orig = @(nsnp_idx) orig_idxs_in_nsnp_frame(nsnp_idx);
ori2nsnp = @(ori_idx) nsnp_idxs_in_ori_frame(ori_idx);
nsnp2ori = @(nsnp_idx) ori_idxs_in_nsnp_frame(nsnp_idx);
nsnp2oriv = @(nsnp_idx) oriv_idxs_in_nsnp_frame(nsnp_idx);
%nsnp2ori_rc = @(nsnp_idx) ind2sub(size(ori), ori_idxs_in_nsnp_frame(nsnp_idx));

% Output Struct
frame.orig = ospl; 
frame.oriv = oriv; 
frame.nsnpl = nsnpl;
frame.ori = ori; 
frame.nsnp_idxs_in_orig_frame = nsnp_idxs_in_orig_frame;
frame.orig_idxs_in_nsnp_frame = orig_idxs_in_nsnp_frame;
frame.oriv_idxs_in_nsnp_frame = oriv_idxs_in_nsnp_frame;
frame.nsnp_idxs_in_oriv_frame = nsnp_idxs_in_oriv_frame;
frame.nsnp_idxs_in_ori_frame = nsnp_idxs_in_ori_frame;
frame.orig_idxs_in_ori_frame = orig_idxs_in_ori_frame;
frame.ori_idxs_in_orig_frame = ori_idxs_in_orig_frame;
frame.ori_alpha_nums = ori_alpha_nums;
frame.nsnp_alpha_nums = nsnp_alpha_nums;
frame.orig_alpha_nums = orig_alpha_nums;
frame.step_boundaries = step_boundaries;

c.orig2nsnp = orig2nsnp;
c.nsnp2orig = nsnp2orig;
c.ori2nsnp = ori2nsnp;
c.nsnp2ori = nsnp2ori;
c.nsnp2oriv = nsnp2oriv;


