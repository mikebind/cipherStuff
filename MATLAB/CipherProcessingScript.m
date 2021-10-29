% Cipher Script

%% Load CipherText
ospl = fileread('CipherTextWithLineBreaks.txt');
orig_idxs = 1:length(ospl);

%% Build no-space no-punct no-numbers vector
ospl_as_integers = cast(ospl, 'uint8');
% newlines are 10, for future reference
% Ciphertext lowercase letters are 97-122, uppercase letters are 65-90
lower_mask = ospl_as_integers > 96 & ospl_as_integers < 123;
upper_mask = ospl_as_integers > 64 & ospl_as_integers < 91;
nsnp_mask = lower_mask | upper_mask; 
orig_nonletter_mask = ~nsnp_mask;
orig_idxs_in_nsnp_frame = orig_idxs(nsnp_mask); % holds the indices of nsnp characters in the original text
nsnp = ospl(orig_idxs_in_nsnp_frame);
nsnpl = lower(nsnp); % all lowercase version

%% %%%% Initial alignment attempts  %%%% %%
% Initial thinking on alignment is to use repeated words or character
% sequences, because these very likely represent repetitions of
% the encipherment scheme. 

%% WORDS
% We can look at words by making a version in the original frame which
% replaces all non-letters with underscores. Then, we can treat an
% underscore-letter sequence as the start of a word, and a
% letter-underscore seqence as the end of a word. 
o_ = lower(ospl); % copy and lowercase original
o_(orig_nonletter_mask) = '_'; % replace non-letters as underscores

repeated_words = find_repeated_words(o_);
% repeated_words is a cell array, where each cell is of the format
% {'word', [start1, start2, start3,...]} where start1, etc. are the
% original ospl indices where the repeated word begins. There are as many
% start locations as there are repetitions.

%%
% There are 57 repeated words, eleven of which are repeated 3 times, the
% rest twice. Words are likely more reliable than character sequences
% (though longer character sequences are also likely quite reliable) as
% flags of true repetition.  Let's see how far we might be able to get from
% just this word data. 

% Cycling appears in the nsnp frame (i.e. you advance to the next cipher
% alphabet only when a letter is enciphered), so we really want to be
% looking at differences in nsnp frame indexes when we are considering
% relative alignment. For this, it makes sense to have a reference to the
% nsnp index in the original text frame.
nsnp_idxs_in_orig_frame = zeros(size(ospl));
nsnp_idxs_in_orig_frame(nsnp_mask) = 1:length(nsnp);

% I want to know how far apart each repetition is and mod this by 18
% Let's make a table with columns: word, nsnp_start, repeat_nsnp_start,
% difference
words = {};
orig_starts = [];
orig_reps = [];
for idx = 1:length(repeated_words)
    word = repeated_words{idx}{1};
    ospl_locs = repeated_words{idx}{2};
    for rep_idx = 1:length(ospl_locs)-1
        start = ospl_locs(rep_idx);
        for fin_idx = rep_idx+1:length(ospl_locs)
            fin = ospl_locs(fin_idx);
            % add info for row
            words{end+1,1} = word; 
            orig_starts(end+1,1) = start;
            orig_reps(end+1,1) = fin;
        end
    end
end
nsnp_starts = (nsnp_idxs_in_orig_frame(orig_starts))';
nsnp_reps = (nsnp_idxs_in_orig_frame(orig_reps))';
T = table(words,nsnp_starts, nsnp_reps);
T.nsnp_diff = T.nsnp_reps-T.nsnp_starts;
T.diff18 = mod(T.nsnp_diff,18);

%% Proving 18 is special
% If we plot the difference vs the spacing mod 18, we see a lot of
% structure
figure; 
subplot(1,2,1);
plot(T.nsnp_diff, T.diff18,'o')
title('Spacing mod18 vs Spacing')
subplot(1,2,2);
plot(T.nsnp_diff, mod(T.nsnp_diff,16),'o');
title('Spacing mod16 vs Spacing')
% Way less structure

% Also, note that the smallest spacings for repeats are one at 18, 5 at 36,
% two at 54, and one at 90. These are all multiples of 18. 

%% 
% The sequence seems to be to use the same frame for some period, then
% shift forward 14 alphabets in the cycle (or, equivalently and perhaps
% more likely, shift backwards 4 alphabets).  The basic evidence is that it
% is definitely possible to stay in one frame for at least 90 characters.

% There appear to be approximately 9 shifts represented in the data. There
% are also about 9 paragraphs in the data, so one possibiliy would be -4
% alphabets every time you start a new paragraph (this would also provide a
% justification for leaving the paragraph formatting in place!). However,
% the frequency of changing looks sort of regular, so perhaps it is after
% some number of characters translated.  We could get a guess at this by
% looking at the slope between the middles of steps. Choosing the middle of
% a couple steps by eye, I get (1500-375)/3 = 375.  Going over a longer
% distance, I get (2600-375)/6 = 371.  That seems really regular.
% Paragraphs, on the other hand, seem to range from ~200 characters to ~850
% characters.  One possibility to keep in mind would be if it were every
% 360 characters (20 times through the 18 alphabet loop). 

% I am surprised that there doesn't seem to be much mixing at the borders of
% the steps.  If the shifts usually or always happened at, say 300
% characters, then I would expect some repeats that were ~200 characters
% apart to cross a step boundary, and some to not cross a step boundary.
% But, in general, I don't see this happening, which I find somewhat hard
% to explain. 

%% Anomalies
context = @(n_idx) ospl(orig_idxs_in_nsnp_frame(n_idx)-20:orig_idxs_in_nsnp_frame(n_idx)+20);
% anon func to show original context around a given nsnp frame idx
context2 = @(n_idx) fprintf('%s|%s\n',ospl(...
    orig_idxs_in_nsnp_frame(n_idx)-20 : orig_idxs_in_nsnp_frame(n_idx)-1),...
    ospl(orig_idxs_in_nsnp_frame(n_idx):orig_idxs_in_nsnp_frame(n_idx)+20))
% notation: 871n means index 871 in the nsnp frame. 871o would mean in the
% original frame. 
% 'wq', 16 apart at 871n and 1049n.  Perhaps spurious repetition?
% 'ik', 3 apart at 2209n and 2698n.  Very likely spurious, it is an
%     odd difference
% 'n', 12 apart at 52n and 1567n. Definitely spurious, first one looks like
%     the word 'a' and the second one looks like the capital letter 'i'
% 'h', 8 apart at 220n and 1812n. Definitely spurious, first one looks like
%     's' in 90s and the second one looks like word 'a'.
% 'oy, 8 apart at 1321n and 3003n. Not clearly in error
% 'c', 7 apart at 207n and 1906n. Definitely spurious, 1880c and dnorj'c,
%     first is clearly 's' and second is clearly 't'

% So, only 6 anomalies of 57, and of those 6, 3 are clearly spurious, and
% the other 3 are only two letters (so could more easily arise by chance).
% None of the anomalies appear to provide special insight at this point,
% and I think likely arose by chance. 


%% How to help build islands or identify boundaries from data?
% How about we translate all the non-anomalous data into a number of -4
% alphabet steps, and then plot a horizontal colored line starting at the
% start, ending at the repeat, with y value governed by start and color
% governed by number of steps (need 9 step possibilities).  
% 0 is 0 steps
% 14 is 1
% [0,14,10,6,2,16,12,8,4,0]
% Colors RGBCMYK, orange, maroon, dark green 
cmap = [1,0,0;
    0,1,0;
    0,0,1;
    0,1,1;
    1,0,1;
    1,1,0;
    0,0,0;
    1,0.5,0;
    0.5,0,0;
    0,0.5,0];

% Sort by ascending spacing
TbyDiff = sortrows(T,'nsnp_diff');
% Remove anomalies from this table
anom_words = {'wq','ik','n','h','oy','c'};
for anom_idx = 1:length(anom_words)
    TbyDiff(strcmp(anom_words{anom_idx},TbyDiff.words),:) = [];
end

steps_apart = zeros(size(TbyDiff.diff18));
steps = [0,14,10,6,2,16,12,8,4];
for step_idx = 1:length(steps)
    step_diff = step_idx-1;
    steps_apart(TbyDiff.diff18 == steps(step_idx)) = step_diff;
end
% Fix last entry, which is actually 10 steps apart, not 0
steps_apart(end) = step_diff + 1;

TbyDiff.steps_apart = steps_apart;
% Sort by starting point
TbyStart = sortrows(TbyDiff,2);

% Create figure to show
fig = figure(10);
y = 1;
for row = 1:length(TbyStart.nsnp_starts)
    start = TbyStart.nsnp_starts(row);
    fin = TbyStart.nsnp_reps(row);
    steps = TbyStart.steps_apart(row);
    c = cmap(steps+1,:);
    line([start,fin],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
%% Add verticals every 360
for x = 0:360:3600;
    line([x,x],[0,75],'Color',rgb('pale gray'),'LineStyle','--');
end
xlabel('repeat nsnp start index')
ylabel('word')
title('Word repeat start locations, colored by number of expected boundaries, 360')

% Most gaps cross the appropriate number of transition lines, but there are
% a few exceptions, 7 of 73 just barely fail (see below at "Disproofs..." for exhaustive list)

% For green (1 step), 'nxy' just barely crosses 2 of the 360-spaced lines. 
% This proves that 360, starting at 0, cannot be the proper spacing. What
% kind of bounds can we derive from this sort of info?  Well, if the
% spacing were 300, a difference of 392 (like for 'nxy') would HAVE TO be 2
% or 3 steps apart, but it is actually only one step apart, so the spacing
% can't be 300. Because this example distance of 392 contains only one
% step, then we know that the step distance must be greater than half of
% 392. A one-step difference places no upper bound on step size. 

% However, let's consider a 2-step distance of 766 (as seen for 'ncg').  We
% know that 766 must be large enough to contain 1 full step, so the largest
% a step could be is ~764.  

% If distance D contains N step boundaries which are regularly spaced, then
% the step boundary can be no bigger than D/(N-1), and no smaller than
% D/(N+1)

% This reasoning can be applied to all rows in TbyStart to see how much we
% can narrow the bounds
lowerSpacingBound = TbyStart.nsnp_diff./(TbyStart.steps_apart+1);
upperSpacingBound = TbyStart.nsnp_diff./(TbyStart.steps_apart-1);
upperSpacingBound(upperSpacingBound<0)=Inf; % because 0 step ones also tell us nothing about upper bounds
spacingBounds = [max(lowerSpacingBound), min(upperSpacingBound)]
% bounds come out to be 333 to 416, which isn't really that helpful because
% we had already suspected somewhere in the 350-390 range.

% Disproofs of 360 starting at 0:
% green, 'nxy' 1778 to 2170, boundary at 1800
% blue, 'knr' just barely crosses 3 when it should cross 2 (starts at 2080
% and finishes at 2882, with a boundary at 2880).
% blue 'bv' touches a third boundary when it should cross 2 (starts at 2060
% and ends at 2880, with a boundary 2880). I should think a bit more
% clearly about whether this is acutally a violation or not.
% cyan, 'jfg' just barely crosses 4 with 3 steps (starts at 1078 with
% boundary at 1080)
% yellow, 'vkc' just barely crosses 6 when it should cross 5 (starts at
% 1437, with step predicted at 1440). All other yellows work with 360.
% orange, 'knr' (again), starts at 246 ends at 2882 with boundary at 2880.
% should be 7 but touches 8 at 360 spacing
% The only maroon, 'ke' crosses 9 at 360 when it should cross 8.


%% Interpreting figure 10
% 'zn' repetition is 6 steps apart (black), going from 184n to 2482n, but 
% 'ixb' repetition is 7 steps (orange), going from 200n to 2800n.  This
% suggests that there must be a step transition somewhere between 2482 and
% 2800.  
% 'odv' says there must be a step transition between 2298 and 2640, so this
% narrows the window there down to 2482 to 2640.  And this must be the 7th
% boundary, so bounds would then be narrowed to 355 to 377. 
%
% What if we just try all of these possibiilites and see which ones are
% contradicted?
spacings_to_try = 340:390;
N_probs = [];
for sp_idx = 1:length(spacings_to_try)
    sp = spacings_to_try(sp_idx);
    for i = 1:length(TbyStart.diff18)
        pN(i,sp_idx) = number_of_crossings(TbyStart.nsnp_starts(i), TbyStart.nsnp_reps(i), sp); 
    end
    N_probs(sp_idx) = sum(pN(:,sp_idx)~=TbyStart.steps_apart);
    if N_probs(sp_idx)<5
        fprintf('Current Spacing: %i\n',sp);
        TbyStart(pN(:,sp_idx)~=TbyStart.steps_apart,:)
    end
end
% Ideally, N_probs would drop to zero at some regular spacing, but I don't
% see that.  Instead, it never drops below 3 (at 367 and 370) and 4 at
% [362,364,365,366,368,369,371]. That does seem to be the optimal region,
% though, with counts climbing to the steady double digits below 356 and
% above 379.  Maybe there is some mild irregularity that can fix the few
% remaining cases (like leap years? i.e. 366 sometimes instead of 365?).

% Especially concerning among the remaingin problems is 'dnhq' which is a
% 0-step spacing only 36 characters apart.  There is definitely no step in
% that range.

%% 
% Let's create a new version of figure 10 using 365 as the spacing to
% examine the problems in more detail
fig = figure(11);
clf(fig)
y = 1;
% Set colors based on match or not instead of based on steps_apart
sp = 365;
for row = 1:length(TbyStart.nsnp_starts)
    start = TbyStart.nsnp_starts(row);
    fin = TbyStart.nsnp_reps(row);
    steps = TbyStart.steps_apart(row);
    pred = number_of_crossings(start,fin, sp); 
    if pred == steps
        c = rgb('pale gray');
    else
        c = rgb('red');
    end
    line([start,fin],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
% Add verticals every 365
for x = 0:sp:3750
    line([x,x],[0,75],'Color',rgb('pale gray'),'LineStyle','--');
end

xlabel('repeat nsnp start index')
ylabel('word')
title('Word repeat start locations, red=not expected number of steps, 365')

%% Manual boundaries
% 
basic_stepsize = 365;
step_boundaries = basic_stepsize:basic_stepsize:3750;

% Then move some to try to eliminate all problematic boundaries
% 1095 (3rd boundary) needs to move back to < 1078 and > 972  (-17 to -123, /3)
% 1460 (4th boundary) needs to move back to < 1437 and > 1402 (-23 to -58, /4) 
% 1825 (5th boundary) needs to move back to < 1813 and > 1778 (-12 to -47, /5) 
% 2555 (7th boundary) needs to move back to < 2524 and > 2484 (-31 to -71, /7)

% Looks from this like an offset of about 32 to 47 might work.  (Also might
% not because moving other boundaries could cause problems, but worth
% checking!
offset = -32; 
step_boundaries = step_boundaries + offset;

fig = figure(12);
y = 1;
% Set colors based on match or not instead of based on steps_apart
for row = 1:length(TbyStart.nsnp_starts)
    start = TbyStart.nsnp_starts(row);
    fin = TbyStart.nsnp_reps(row);
    steps = TbyStart.steps_apart(row);
    len = length(TbyStart.words(row));
    pred = find_num_crossings(start, fin, len, step_boundaries);
    %pred = number_of_crossings(start,fin, sp); 
    if pred == steps
        c = rgb('pale gray');
    else
        c = rgb('red');
    end
    line([start,fin],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
% Add verticals at boundaries
for x = step_boundaries %0:sp:3750
    line([x,x],[0,75],'Color',rgb('pale gray'),'LineStyle','--');
end
xlabel('repeat nsnp start index')
ylabel('word')
title('Adding Offset -32, Word repeat start locations, red=not expected number of steps, 365')


% This approach (365 spacing, 32 offset) ALMOST works, but there is one
% remaining problem which I think can't be rectified with just an offset.

%% Explore offsets...
basic_stepsize = 365;
step_boundaries = basic_stepsize:basic_stepsize:3750;
offset = -36; 
step_boundaries = step_boundaries + offset;

fig = figure(13);
clf(fig);
y = 1;
% Set colors based on match or not instead of based on steps_apart
for row = 1:length(TbyStart.nsnp_starts)
    start = TbyStart.nsnp_starts(row);
    fin = TbyStart.nsnp_reps(row);
    steps = TbyStart.steps_apart(row);
    len = length(TbyStart.words(row));
    pred = find_num_crossings(start, fin, len, step_boundaries);
    %pred = number_of_crossings(start,fin, sp); 
    if pred == steps
        c = rgb('pale gray');
    else
        c = rgb('red');
    end
    line([start,fin],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
% Add verticals at boundaries
for x = step_boundaries %0:sp:3750
    line([x,x],[0,75],'Color',rgb('pale gray'),'LineStyle','--');
end
xlabel('repeat nsnp start index')
ylabel('word')
title('Word repeat start locations, red=not expected number of steps, 365, -36 offset')

% There is a single remaining problem in 365/31 to 365/38, which is 'nxy'
% crossing both 5th and 6th boundaries when it is only supposed to cross
% one of the two.

% nxy is at 1778 to 2170 (392 length), and boundaries in 360/36 are at 1789
% and 2154.  Any offset larger enough to repair this for nxy causes more problems
% elsewhere.  Also, another word with rep starts at 1772, so the window for 
% placement seems exceedingly narrow. What if we try just moving only that
% boundary to between 1772 and 1778 (let's say 1776 to give the 1772 room
% for it's word, which is only 2 letters).
%% Manually adjust boundary 5 AND 8
step_boundaries(5) = 1776; % moving one boundary 13 letters back (must be between
% 1772 ('gy') and 1778 ('nxy')
step_boundaries(8) = 2886; % moving forward 2 letters to allow all of knr in the same frame (was kn|r)
step_boundaries(2) = 672; % moving back 20 to accomodate 'etwa' sequence
step_boundaries(3) = 1000; % moving back 58 to accomodate 'uhez' sequence
step_boundaries(10) = 3610; % moving back to accomodate 'ke' word with 'etwa' seq
basic_stepsize = 365;
step_boundaries = basic_stepsize:basic_stepsize:3750;
offset = -36; 
step_boundaries = step_boundaries + offset;
step_boundaries(5) = 1776; % moving one boundary 13 letters back (must be between


fig = figure(14);
clf(fig);
y = 1;
% Set colors based on match or not instead of based on steps_apart
for row = 1:length(TbyStart.nsnp_starts)
    start = TbyStart.nsnp_starts(row);
    fin = TbyStart.nsnp_reps(row);
    steps = TbyStart.steps_apart(row);
    len = length(TbyStart.words(row));
    pred = find_num_crossings(start, fin, len, step_boundaries);
    %pred = number_of_crossings(start,fin, sp); 
    if pred == steps
        c = rgb('pale gray');
    else
        c = rgb('red');
    end
    line([start,fin],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
% Add verticals at boundaries
for x = step_boundaries %0:sp:3750
    line([x,x],[0,75],'Color',rgb('pale gray'),'LineStyle','--');
end
xlabel('repeat nsnp start index')
ylabel('word')

title('Word repeat start locations, 365, -36 offset, bnd(5)=1776, bnd(8)=2886')

% This leaves zero problem words (though it does leave the boundaries
% themselves somewhat suspect).  However, I think this means that we should
% be able to correctly orient nearly all of the text, understanding that
% there are likely to be some errors near boundaries.  If we even allowed a
% margin of 20 characters to each side of a boundary might be questionable,
% that still would mean that we've confidently oriented (365-40)/365 = 90%
% of the text, which is much better than I've gotten before.

%% Build oriented frame
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
padLength = ceil(length(oriv)/18)*18 - length(oriv); % 3, same padding needed for nsnp wrapping
pad_spaces = repmat(' ', [1,padLength]);
oriv = [oriv,pad_spaces];
% For a character whose nsnp_idx = 10, that character's location in the ori
% vector is ori_idxs_in_nsnp_frame(10).
% To go the other way (i.e. to find the nsnp index of a character at ori
% vector location ori_idx), we want nsnp_idxs_in_oriv_frame(ori_idx).
% Non-nsnp locations can have zero for nsnp index
nsnp_idxs_in_oriv_frame = zeros(size(oriv));
nsnp_idxs_in_oriv_frame(oriv~=' ') = 1:length(nsnp);

% Wrap to Nx18, but want to go across rows instead of down columns, do this
% by doing down columns to make 18xN, then transpose.
ori = reshape(oriv,18,[])';
nsnp_idxs_in_ori_frame = reshape(nsnp_idxs_in_oriv_frame, 18,[])';

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
ori_alpha_nums = repmat(1:18,size(ori,1),1);
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


%% Word alignment verification
% repeated_words has orig_frame locations, so we need to translate those to
% nsnp_frame locations, and then ori_frame locations (==alphabet)
% Actually, we can just use the TbyStart table and verify that
%find([nsnp_alpha_nums(TbyStart.nsnp_starts)'-nsnp_alpha_nums(TbyStart.nsnp_reps)']~=0)

fig = figure(15);
clf(fig);
y = 1;
% Set colors based on match or not instead of based on steps_apart
prob_idxs = [];
for row = 1:length(TbyStart.nsnp_starts)
    start = TbyStart.nsnp_starts(row);
    fin = TbyStart.nsnp_reps(row);
    steps = TbyStart.steps_apart(row);
    %pred = find_num_crossings(start, fin, len, step_boundaries);
    %pred = number_of_crossings(start,fin, sp); 
    if nsnp_alpha_nums(start)==nsnp_alpha_nums(fin)% pred == steps
        c = rgb('pale gray');
    else
        c = rgb('red');
        prob_idxs(end+1,1) = row;
        fprintf('%s\n', TbyStart.words{row});
    end
    line([start,fin],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
% Add verticals at boundaries
for x = step_boundaries %0:sp:3750
    line([x,x],[0,75],'Color',rgb('pale gray'),'LineStyle','--');
end
xlabel('repeat nsnp start index')
ylabel('word')
title('Verification of alphabet number consistency')


%% Repeated character sequences to possibly refine step_boundaries
matches = find_repeated_char_seqs(nsnpl,4); 
numFourCharRepeats = length(matches);
%%
for match_idx = 1:length(matches)
    numberOfPlaces = matches{match_idx};
    alpha1 = nsnp_alpha_nums(numberOfPlaces{2});
    alpha2 = nsnp_alpha_nums(numberOfPlaces{3});
    if alpha1==alpha2
        fprintf('Match for %s\n', numberOfPlaces{1});
    else
        fprintf('Not a match for %s: a1=%i, a2=%i, loc1=%i, loc2=%i\n',numberOfPlaces{1},alpha1,alpha2,numberOfPlaces{2},numberOfPlaces{3});
    end
end
% Almost all of these initially were not a match, including those which represent
% words from earlier, so there was a bug in my reasoning somewhere above,
% we needed to go back and fix things to make sure that all of the words are
% verified aligned to the same alphabet (which is done)
%% Plot for char seqs
fig = figure(16);
clf(fig);
y = 1;
% Set colors based on match or not instead of based on steps_apart
prob_idxs = [];
for match_idx = 1:length(matches)'
    numberOfPlaces = matches{match_idx};
    start = numberOfPlaces{2};
    rep = numberOfPlaces{3};
    alpha1 = nsnp_alpha_nums(start);
    alpha2 = nsnp_alpha_nums(rep);
    if alpha1==alpha2
        c = rgb('pale gray');
    else
        c = rgb('red');
        prob_idxs(end+1,1) = match_idx;
        fprintf('%s, %i, %i, %i, %i\n', numberOfPlaces{1},start, rep, alpha1, alpha2);
    end
    line([start,rep],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
% Add verticals at boundaries
for x = step_boundaries %0:sp:3750
    line([x,x],[0,75],'Color',rgb('pale gray'),'LineStyle','--');
end
xlabel('repeat nsnp start index')
ylabel('4-char sequence')
title('Four-Character Seq consistency check')

%% Build match4T table
char4 = cell(length(matches),1);
starts = zeros(length(matches),1);
reps = zeros(length(matches),1);
for match_idx = 1:length(matches)
    numberOfPlaces = matches{match_idx};
    char4{match_idx,1} = numberOfPlaces{1};
    starts(match_idx) = numberOfPlaces{2};
    reps(match_idx) = numberOfPlaces{3};
end
match4T = table(char4,starts,reps);
match4T.steps_apart = repmat(-1,size(match4T.starts));
steps = [0,14,10,6,2,16,12,8,4];
for step_idx = 1:length(steps)
    step_diff = step_idx-1;
    mod18 = mod(match4T.reps-match4T.starts,18);
    match4T.steps_apart(mod18 == steps(step_idx)) = step_diff;
end
num_cross = repmat(-1,size(starts));
for r = 1:length(num_cross)
    num_cross(r,1) = find_num_crossings(match4T.starts(r), match4T.reps(r), length(match4T.char4(r)), step_boundaries);
end
match4T.num_cross = num_cross;
% Display those with mismatches
match4T(match4T.num_cross~=match4T.steps_apart,:)
% Of these mismatches, only those with one (or at most two) boundary
% differences are possibly remediable by adjusting a boundary. 
% 'xmqo' is two apart (crosses 6 but should only cross 4 by spacing) but
% not near either boundary, so probably spurious.
% 'bfon' and 'ynhn' are similar (2 steps, but not very close on both ends)
% 'plvk' looks like it should cross 5 but only crosses 4 of the current
% boundaries, and is very close on the starting end (670 vs boundary at
% 694), but that's the wrong direction, if we nudged the boundary below
% 670, then it would only cross 3, not 5, and the other boundary is not
% close.
% 'etwa' currently crosses two, but looks like it should only have one step
% apart by spacing, and is quite close to a boundary (674 vs boundary at
% 694). If boundary was nudged down 20 spaces, then agreement could be
% reached (though of course it would need to be checked if other words were
% thrown off)
% 'uhez' currently crosses 5 but looks like it should only cross 4, and is
% about 60 letters from boundaries at both ends (it is at 1001 and 2569,
% boundaries at 1059 and 2519). To cross one less, 2519 would need to move
% to at least 2573; or 1059 would need to drop to at least 1001. Context
% for 'uhez' looks very strong, even though boundary shift seems a bit
% large.
% 'uoyj' currently crosses 4 but looks like it should only cross 3.
% However, boundaries would need to move far too much to accomadate this.
% Context is plausible, but not overwhelming
% All other candidates are not reconcilable (steps apart is not even or
% difference between num_cross and steps_apart is >2)

%% Try accomodating 'etwa' and 'uhez' by moving boundary
% Check what other problems are caused, if any

% Moving boundary 2 to 674 from 694 caused no new problems, and fixed
% 'etwa'.

% Moving boundary 3 to 1001 caused no new problems, and fixed 'uhez'. Have
% not checked moving the other boundary...

%% What bounds are there on the step boundaries?
match4Tvalid = match4T(match4T.steps_apart==match4T.num_cross,:)
anchors = cat(1,match4Tvalid.starts(:),match4Tvalid.reps(:),TbyStart.nsnp_starts(:), TbyStart.nsnp_reps(:));
anchors = unique(anchors);

for bnd_idx = 1:length(step_boundaries)
    bnd =step_boundaries(bnd_idx);
    lowerBnd(bnd_idx) = max(anchors(anchors < bnd));
    eqBnd(bnd_idx) = any(bnd==anchors);
    upperBnd(bnd_idx) = min(anchors(anchors > bnd));
end

[lowerBnd',upperBnd',eqBnd',step_boundaries']

%% Let's make a unified table of all anchor elements
elem = cat(1,TbyStart.words,match4Tvalid.char4);
first = cat(1,TbyStart.nsnp_starts, match4Tvalid.starts);
rep = cat(1,TbyStart.nsnp_reps, match4Tvalid.reps);
uT = table(elem, first, rep);

uT.steps_apart = repmat(-1,size(rep));
for step_idx = 1:length(steps)
    step_diff = step_idx-1;
    mod18 = mod(uT.rep-uT.first,18);
    uT.steps_apart(mod18 == steps(step_idx)) = step_diff;
end

uT = sortrows(uT, 2);
uTr = sortrows(uT,3);

%% Play with boundaries and view in plots
step_boundaries(5) = 1750;
step_boundaries(7) = 2480;
step_boundaries = (380:380:3817)-87;
% No boundary allowed at 1814-1853
% (380:380:3817)-87 has only two problems, ke and rpsd. Evidence
% on rpsd is VERY strong.
%step_boundaries = (378-108):378:3817;

% HERE is a set of step boundaries which are all multiples of 18
step_boundaries(10) =3600; %=200*18 (can't do 199 or 201 though 201 might be possible if also shift boundary 2 above 673+2)
step_boundaries(9) = 3258; %=181*18 (178-182 works OK)
step_boundaries(8) = 2898; %=161*18 (161-163 works OK)
step_boundaries(7) = 2484; %=138*18 (only 138 works cleanly, 139-140 conflicts only with gy)
step_boundaries(6) = 2142; %119*18 (119-120 works OK)
step_boundaries(5) = 1764; %=98*18 (95-98 works OK; 99 conflicts with gy and nxy, gap beween gy and nxy is 1774 to 1778)
step_boundaries(4) = 1404; %=78*18 (78-79 works OK)
step_boundaries(3) = 990; %=55*18 (only 55 works OK)
step_boundaries(2) = 648; %=36*18 (35-37 works OK)
step_boundaries(1) = 324; %=18*18  (15-24 work OK)
bnd_mult18 = step_boundaries/18;


[f,c] = build_oriented_frame(ospl, step_boundaries);

fig = figure(17);
clf(fig);
y = 1;
fprintf('\n By start order:\n')
% Set colors based on match or not instead of based on steps_apart
for match_idx = 1:size(uT,1)
    start = uT.first(match_idx);
    rep = uT.rep(match_idx);
    alpha1 = f.nsnp_alpha_nums(start);
    alpha2 = f.nsnp_alpha_nums(rep);
    if alpha1==alpha2
        c = rgb('pale gray');
    else
        c = rgb('red');
        fprintf('%s, %i, %i, %i, %i\n', uT.elem{match_idx},start, rep, alpha1, alpha2);
    end
    line([start,rep],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
% Add verticals at boundaries
for x = step_boundaries %0:sp:3750
    line([x,x],[0,120],'Color',rgb('pale gray'),'LineStyle','--');
end
xlabel('repeat nsnp start index')
ylabel('repeated element')
title('Unified table consistency check')
ax17 = gca;
ax17.XTick = step_boundaries;
%
fig = figure(18);
clf(fig);
y = 1;
fprintf('\n By repeat order:\n')
% Set colors based on match or not instead of based on steps_apart
for match_idx = 1:size(uTr,1)
    start = uTr.first(match_idx);
    rep = uTr.rep(match_idx);
    alpha1 = f.nsnp_alpha_nums(start);
    alpha2 = f.nsnp_alpha_nums(rep);
    if alpha1==alpha2
        c = rgb('pale gray');
    else
        c = rgb('red');
        fprintf('%s, %i, %i, %i, %i\n', uTr.elem{match_idx},start, rep, alpha1, alpha2);
    end
    line([start,rep],[y,y],'Color',c,'LineWidth',2);
    y = y+1;
end
% Add verticals at boundaries
for x = step_boundaries %0:sp:3750
    line([x,x],[0,120],'Color',rgb('pale gray'),'LineStyle','--');
end
xlabel('repeat nsnp start index')
ylabel('repeated element')
title('Unified table consistency check')
ax18 = gca;
ax18.XTick = step_boundaries;

step_boundaries
diff(step_boundaries/18)
%%
% ixb and ixp are in the same frame (start on alphabet 2), so there are
% actually 6 repetitions of that 

forbidden = [ % No boundaries allowed between these pairs of locations
    8, 134;
    1101, 1217;
    1384, 1402;
    1813, 1850;
    1962, 2016;
    2094, 2130;
    2728, 2854;
    2999, 3089;
    3444, 3480];
figure(20);
clf(20);
for y = 1:size(forbidden,1)
    start = forbidden(y,1);
    fin = forbidden(y,2);
    line([start,fin],[y,y], 'Color',rgb('gray'), 'LineWidth',2);
end
for x = step_boundaries %0:sp:3750
    line([x,x],[0,y+5],'Color',rgb('pale gray'),'LineStyle','--');
end
set(gca,'XTick',step_boundaries);
    
%% Systematically investigate sensible step boundary options
% intervals = 360+(-2:2)*18; % integer number of cycles
% offsets = 18*(-15:0); % with an offset which is a multiple of 18, just different number of cycles in first time
% best is 378 - (126, 108, 90, 72), all with 6 problems each

% intervals = [360,378];
% offsets = -9:9;
% best is 360 + [3,4] with 6 problems each

% intervals = [360:380];
% offsets = [-100:10:100];
% best is 380 - 90 with only 2 problems

% intervals = 375:400;
% offsets = [-150:5:0];
% best is 380-90, 381-95, 382-100, 383-110, 384-115, 385-125

intervals = [378,380];
offsets = -100:-70;


output = zeros(length(offsets),length(intervals));
nonmatches = cell(size(output));
for interval_idx = 1:length(intervals)
    interval = intervals(interval_idx);
    for offset_idx = 1:length(offsets)
        offset = offsets(offset_idx);
        % Construct step boundaries
        step_boundaries = ((interval+offset):interval:3817);
        % Reconstruct frame
        [frame,~] = build_oriented_frame(ospl, step_boundaries);
        % Check uT entries for consistency
        nonmatch_mask = frame.nsnp_alpha_nums(uT.first) ~= frame.nsnp_alpha_nums(uT.rep);
        output(offset_idx, interval_idx) = sum(nonmatch_mask);
        nonmatches{offset_idx, interval_idx} = uT.elem(nonmatch_mask);
        
%         % Check for boundaries in forbidden zones
%         if bound_in_forbidden(step_boundaries, forbidden)
%             % Reject
%             output(offset_idx, interval_idx) = 100; % means rejected due to boundary location in forbidden
%         else
%             % Reconstruct frame
%             [frame,~] = build_oriented_frame(ospl, step_boundaries);
%             % Check uT entries for consistency
%             nonmatch_mask = frame.nsnp_alpha_nums(uT.first) ~= frame.nsnp_alpha_nums(uT.rep);
%             output(offset_idx, interval_idx) = sum(nonmatch_mask);
%             nonmatches{offset_idx, interval_idx} = uT.elem(nonmatch_mask);
%         end
    end
end

output
minProb = min(output(:))
[r,c] = find(output==minProb);
best = [intervals(c)', offsets(r)']

%% Locking in on these boundaries for now
% HERE is a set of step boundaries which are all multiples of 18
step_boundaries(10) =3600+18*8; %=200*18 (can't do 199 or 201 though 201 might be possible if also shift boundary 2 above 673+2)
step_boundaries(9) = 3258+36; %=181*18 (178-182 works OK)
step_boundaries(8) = 2898; %=161*18 (161-163 works OK)
step_boundaries(7) = 2484+18*5; %=138*18 (only 138 works cleanly, 139-140 conflicts only with gy)
step_boundaries(6) = 2142+18; %119*18 (119-120 works OK)
step_boundaries(5) = 1764+18; %=98*18 (95-98 works OK; 99 conflicts with gy and nxy, gap beween gy and nxy is 1774 to 1778)
step_boundaries(4) = 1422+18; %=79*18 (79 works OK, 78 clips in the middle of 'ehl' repeat)
step_boundaries(3) = 990+18*6; %=55*18 (only 55 works OK)
step_boundaries(2) = 648+18*7; %=36*18 (35-37 works OK)
step_boundaries(1) = 378; %342; %=19*18  (15-24 work OK)
bnd_mult18 = step_boundaries/18;

% Advantages: mulitples of 18, no inconsistencies with unified table
% Disadvantages: not evenly spaced, some boundaries have lots of wiggle
% room.  
% Strategy: Use these for now, and hope that 

[f,c] = build_oriented_frame(ospl, step_boundaries);


%% Histograms of letter frequencies for each alphabet
[orihist, percenthist] = ori_histo(f.ori);

%% The following commentary was for an older set of boundaries
% These histograms are discouragingly flat. There should be a clear peak
% for 'e' and likely zeros counts for infrequent letters (z, x, j).
% Instead, all letters appear in the ciphertext at least once, and the
% maximum percentage is only 5.7% to 9%, when it should be more like 13%
% for plaintext 'e'. This strongly suggests that multiple ciphertext
% letters are used for at least 'e'.  The absence of low troughs also
% suggests that they have perhaps been lumped into other symbols. There are
% 210 to 214 ciphertext letters in each alphabet sample, so the expected
% frequency should be approximately double the percentage (200 * 5% = 10
% occurrences expected). The minimum percent ranges from 0.5% to 1.9%.
% There are 4 letters with typical percentages below 0.5%, zxqj, so it
% seems likely that these might have been merged with other letters.  For
% example, if the same ciphertext symbol were used for b and j, the
% frequency of that symbol would be 1.5%+0.15% = 1.65%, basically like b,
% and there would be no symbol for j only. There are just a few more
% letters with typical frequencies at or below 2%: v (1%), b (1.5%), g
% (2%), p (1.9%), y (2%). Others are also probably not very distinguishable
% from those (f (2.2%), m (2.4%), w (2.4%) and u (2.8%) and c (2.8%)). 

% If the encoder wanted to hide the high and low values, what are the
% options?  3 e's would lower % to about 4%. 2 t's and 2 a's, would lower
% each to about 4%. This would use up 4 extra symbols (2 extra for e, 1
% extra each for a and t), which could be used to hide the 4 lowest
% frequency letters zqjx.

%% With newer set of boundaries...
% Histograms are much more sensible looking!

% ixb and ixp start in same alphabet (2), so suspect that b and p in
% alphabet 4 both represent 'e'.
alph = ['abcdefghijklmnopqrstuvwxyz'];

% Together, these ciphertext letters are 37/212 = 17.4% of all letters.
% They do not strictly alternate (also there are 24 'p' and only 13 'b' in
% f.ori(:,4)) so it seems like the encipherer might get to choose which to
% use.

% Still considering ixb and ixp as plaintext 'the' starting in alphabet 2,
% 'x' would be plain 'h' in alphabet 3, which has a frequency of 4.2%,
% which is consistent with 'h'.
% 'i' would be plain 't' in alphabet 2, which has a frequency of 10.4%,
% also consistent with 't'.  

% Most cipher alphabet histograms have at least 1 cipher letter which does
% not appear (1 has none, 7 have one missing letter, 6 have two missing
% letters, and 3 have three missing letters). 

find_nsnp_from_orig_search('Wjxkx Rys A',f)

%% Time to start making guesses

% I want a record of substitutions which a subsMat can be built from and
% which will allow backwards investigation of what let to a particular
% letter getting translated in a particular way

% What information would be needed?
% Starting nsnp_idx, plaintext string

% What else should be recorded for convenience and reference?
% affected alphabet numbers, affected ciphertext letters

% This might be best kept track of with an expanded version with one line
% per letter.  In that case, the expanded version could keep track of which
% entry it was linked to (for back-referencing)

subs = {
    {200, 'the'};
    {203, 's'};
    {204, 'and'};
    {207, 's'};
    {210, 'tothe'};
    ...{215, 'early'};
    {220, 's'};
    {255, 'worldwari'};
    {128, 'worldwari'};
    {568, 'tha'}; % probably that or than
    {1377, 'howeverthe'};
    {1962, 'the'};
    {2080, 'the'};
    {1226, 'worldwari'};
    };
% Build subs cells into expanded table form
[subT, subs] = expandSubsToTable(subs, f);
% Build the word subs table
nsnp_start = cellfun(@(C) C{1}, subs);
plain = cellfun(@(C) C{2}, subs, 'UniformOutput',false);
ciph = cellfun(@(c) c{3}, subs, 'UniformOutput',false);
wsubT = table(nsnp_start, plain, ciph);
% 
subsMat = buildSubsMat(wsubT, f);
% Build the translated versions
tr = applySubsMat(subsMat, f);

% Show translated orig
%tr.oslash
tr.interlaced
%% Alphabet regularities
% looking through the alphabets there seems to be a regular pattern to the
% substitutions.  Assembling those into a common frame, I get:
common = 'er_________vitenalwhsdo___';
% duplicate, so that we have a full sequence starting at any of the first
% 26 locations
common = [common, common];
% Check for consistency with each alphabet
commonSubsMat = subsMat; % copy
for alph_num = 1:18
    current = subsMat(alph_num,:);
    % Find the first non-underscore and non-'e' (because 'e' idx is
    % non-unique in common)
    firstLettIdx = find(current~='_' & current~='e',1,'first');
    firstLett = current(firstLettIdx);
    % Find that letter in the common frame
    commonIdx = find(common==firstLett,1,'first');
    commonStart = commonIdx-firstLettIdx+1;
    if commonStart <= 0
        commonStart = commonStart+26;
    end
    filled = common(commonStart:commonStart+25);
    comp = [current;filled];
    % inconsistencies:
    incon = current~=filled & current~='_' & filled~='_';
    %filled(current~='_')~=filled(current~='_')
    if any(incon)
        incon_idxs = find(incon);
        for idx = 1:length(incon_idxs)
            % Print info about the inconsistency
            currPlainLett = current(incon_idxs(idx));
            commonPlainLett = filled(incon_idxs(idx));
            fprintf('Alphabet #%i: Current has "%s" but common has "%s"\n',alph_num, currPlainLett, commonPlainLett);
            comp
        end
    end
    commonSubsMat(alph_num,:) = filled;
end
% Comp shows that there are just a few possible inconsistencies.  
    
%% 
trc = applySubsMat(commonSubsMat, f);
trc.interlaced

% The result is garbled, but partially readable.  I am going to take that
% as a starting point and work on fixing it.

%% 
% Move definition of step boundaries here to speed iteration...
step_boundaries(10) =3600+18*7; % *8 is too much %=200*18 (can't do 199 or 201 though 201 might be possible if also shift boundary 2 above 673+2)
step_boundaries(9) = 3258+36; %=181*18 (178-182 works OK)
step_boundaries(8) = 2898; %=161*18 (161-163 works OK)
step_boundaries(7) = 2484+18*5; %=138*18 (only 138 works cleanly, 139-140 conflicts only with gy)
step_boundaries(6) = 2142+18; %119*18 (119-120 works OK)
step_boundaries(5) = 1764+18; %=98*18 (95-98 works OK; 99 conflicts with gy and nxy, gap beween gy and nxy is 1774 to 1778)
step_boundaries(4) = 1422+18; %=79*18 (79 works OK, 78 clips in the middle of 'ehl' repeat)
step_boundaries(3) = 990+18*6; %=55*18 (only 55 works OK)
step_boundaries(2) = 648+18*7; %=36*18 (35-37 works OK)
step_boundaries(1) = 360; %342; %=19*18  (15-24 work OK)

step_boundaries = (360-18+1):360:3817;
%step_boundaries(5) = 1800-18;
%step_boundaries(4) = 1440-18;
%step_boundaries(2) = 720-18;
step_boundaries(1) = step_boundaries(1)-5*18;
step_boundaries(2) = step_boundaries(2)-5*18;
step_boundaries(6:10) = step_boundaries(6:10)+54;
[f,c] = build_oriented_frame(ospl, step_boundaries);



new_subs = {
    {1,  'introductionfollowingthewarbetweenthestatestherewasagreatperiodof'};
    {66, 'loggingactivity'};
    {81,  'inthestateofmichigan'};
    {101, 'continuing'};
    {111, 'welluptothetimeofworldwari'};
    {137, 'muchhasbeenwrittenon'};
    {157, 'logginginthepinesandthe'}; % 
    {180, 'lifein'}; 
    {186, 'loggingcampsin'}; % also a little speculative
    {200, 'the'};
    {203, 's'};
    {204, 'and'};
    {207, 's'};
    {208, 'uptothe'};
    {215, 'early'};
    {220, 's'};
    {221, 'the'}; % so far this is 'Rhg', but should be 'The'
    {224, 'periodfromabout'};
    {239, 'toaboutthetimeof'};
    {255, 'worldwari'};
    {264, 'whichmostlyinvolvedhardwoodlogging'};
    {298, 'especially'};
    {308, 'inlowerpeninsulamichigan'};
    {332, 'hasgenerally'};
    {344, 'beenneglected'};
    {357, 'one'};
    {360, 'ofthefrequent'}; 
    {373, 'misconceptionsamong'};
    {392, 'students'};
    {400, 'interestedinmichiganlogginghistoryistoassume'};
    {444, 'thatall'};
    {451, 'logging'};
    {458, 'priortothetimeof'}; % probably 'priortothetimeof'
    {474, 'worldwari'};
    {483, 'wasalike'};
    {491, 'andthatithasbeenwelldocumentedinthematerialofferedonpinelogging'};
    {554, 'itisouropinionthatthoughtherearesimilaritiesbetweenearlypineand'};
    {617, 'laterhardwoodloggingtherearealsoimportantdifferences'};
    {669, 'manyofthesedifferencesareinmattersofimprovedtechniquesandequipment'};
    {735, 'thatgraduallyevolved'};
    {755, 'throughexperience'};
    {772, 'andneedsinthelogging'};
    {792, 'operationitincludesbettercrosscutsawsandtheirmaintenance'};
    {848, 'improvedloggingtongsuseofmoreefficienttechniquesinloadinglogs'};
    {909, 'increaseduseofhorsesratherthanoxentheuseofthebigwheelsforsummer'};
    {972, 'loggingandmoreefficientuseofloggingrailroads'};
    {1016, 'efficiencyandspeed'};
    {1034, 'werealsoevidentatthesawmills'}; % fixed error, used to be "in" the sawmills
    {1062, 'gangsaws'};
    {1070, 'bandsawsand'};
    {1081, 'generalmechanization'};
    {1101, 'ofthemovement'};
    {1114, 'ofthelogsandlumberthroughthemillwerepartoftheimprovements'};
    {1171, 'asoneviewsthepicture'};
    {1191, 'ofchangefrompost'};
    {1207, 'civilwar'};
    {1215, 'tothetimeof'};
    {1226, 'worldwari'};
    {1235, 'thechangesappeargradualandrepresentanevolutionarysequencethatwouldbe'};
    {1303, 'normallyexpectedinanactiveexpanding'};
    {1338, 'industrywhichitwasforsomanyoftheseyears'};
    {1377, 'howeverthe'};
    {1387, 'changesthatwerethemostprofoundinouropinioninvolved'};
    {1437, 'theloggersandpeopleworkinginthewoods'};
    {1473, 'insteadofthehappygolucky'};
    {1497, 'bigpaybigdrunk'};
    {1511, 'andnotomorrowattitudeattributed'};
    {1542, 'tothepineloggertherewasinthis'};
    {1571, 'latterperiodthefarmerswhologgedparttimeforhardcashtoimprovetheir'};
    {1635, 'farm'};
    {1639, 'theyappeartohavebeenmostlyfamily'};
    {1671, 'menorthesonsoffarmersworkingforcashtogetestablishedandsettledown'};
    {1735, 'thesepeoplewereloggingbutalsoplanningtostayandbuildlifeforthemselves'};
    {1803, 'andbecomeapartofacommunitytheywerenotbasicallypart'};
    {1853, 'ofthecutandgetoutphilosophy'};
    {1880, 'oftheearlieryears'};
    {1897, 'thisdoesntmean'};
    {1911, 'thatallofthemenwere'};
    {1930, 'ofthistype'};
    {1940, 'therewerestill'};
    {1954, 'plentyoftheoldtimersthatwereoftenwell'};
    {1991, 'rememberedasisindicatedbythefollowingmaterial'};
    {2036, 'but'};
    {2039, 'the'};
    {2042, 'prevailingattitudeofthetimeshadchanged'};
    {2080, 'thelastvestigeof'};
    {2096, 'someoftheoldattitudesandwildspiritoftheendlesswoodsis'};
    {2149, 'expressedintheactionsandlivesofthatwildbunch'};
    {2193, 'knownasriverhogsorriverdrivers'};
    {2223, 'theywereawildandcolorful'};
    {2247, 'creweven'};
    {2255, 'aftertheturnofthecenturythelastoftheriverdrives'};
    {2302, 'closedthechapteron'};
    {2320, 'thewildspiritofloggingin'};
    {2344, 'southern'}; 
    {2352, 'peninsulamichigan'};
    {2369, 'itwasbusinessanda'};
    {2386, 'jobfromthenon'};
    {2399, 'inordertotryandpreserve'};
    {2422, 'someofthespiritofwhatwasoncelogginginmichigantheauthorsbeganin'};% current 2484 boundary is at the end of this
    {2484, 'tointerviewandrecordconversationswithmenandwomenwhohad'};
    {2538, 'participateddirectlyin'};
    {2560, 'michiganloggingoperationsgenerallyfromaroundupto'}; % this keeps pushing boundary 7 back further
    {2608, 'someinformationhowevergoesback'};
    {2638, 'tothesandevenbefore'};
    {2657, 'wewantedtocapturethesoulofmichigan'};
    {2691, 'loggingbeforetheactorswereallgonefromtheirstage'};
    {2738, 'wefeelthishasbeenaccomplishedinthematerialpresentedandfeelthatthe'};
    {2803, 'excitementthetragedyandthedrudgery'};
    {2837, 'areallpreservedintheverywords'};
    {2866, 'andexpressionsofthepeopleeveryeffortwasmadetoconvey'};
    {2917, 'theirexactwording'};
    {2934, 'andexpressionintheseconversationspresented'}; % fixed espression to expression
    {2976, 'thisistheirstoryastheylivedit'};
    {3005, 'wehavetriedtopresentitinthismatterwiththeleading'};
    {3053, 'questionsandapproachoftheinterview'};
    {3087, 'editedoutsothatonlytheparticipantisspeaking'};
    {3130, 'thisallowsthelifeandexperiencestoflowwithouttheintrusiveinterruptionof'};
    {3200, 'questionsandanswers'};
    {3219, 'thoughthematerialcollectedisprimarilyfrom'};
    {3260, 'benziecountymichigantheattitudesfeelingsandloggingtechniquesrepresented'};
    {3331, 'aretypicaloftheperiodforthewholeregion'};
    {3369, 'ofmichiganandthelivesofpeopleworkinglivingdying'};
    {3416, 'intheirownsetof'};
    {3431, 'environmentalandculturallimitations'};
    {3466, 'hasapplication'};
    {3480, 'andmeaningtoallmeneverywhere'};
    {3508, 'wefeeltheseconversationswillenablethepresentgenerationandfuturegenerations'};
    {3582, 'toenterintoandappreciateapartofthelifeofasmallbutexciting'};
    {3639, 'periodofhistorywhichoccurredinbenziecountymichigan'};
    {3689, 'andwasessentiallyoverwithintwentyyearsafteritstarted'};
    {3741, 'loggingthehardwoods'};
    {3760, 'williamroyoverleasehonor'};
    {3784, 'michigan'};
    {3792, 'edithdymond'};
    {3803, 'overleaseaugust'};
    };

[f36, c36] = build_oriented_frame(ospl, step_boundaries, 36);
% Build subT36
[subT36, new_subs] = expandSubsToTable(new_subs, f36);
% Gold standard based on new_subs alone (frame does not matter)
goldn = repmat('_',size(f.nsnpl));
goldn(subT36.nsnp_idx) = subT36.plain_lett;
goldo = f.orig;
goldo(f.nsnp_idxs_in_orig_frame>0) = goldn;
upper_mask = f.orig~=lower(f.orig);
goldo(upper_mask) = upper(goldo(upper_mask));
%goldo % now shown in figure instead
    
% 
%     {200, 'the'};
%     {203, 's'};
%     {204, 'and'};
%     {207, 's'};
%     {210, 'tothe'};
%     ...{215, 'early'};
%     {220, 's'};
%     {255, 'worldwari'};
%     {128, 'worldwari'};
%     {568, 'tha'}; % probably that or than
%     
%     {1962, 'the'};
%     {2080, 'the'};
%     {1226, 'worldwari'};
%     };

% Build subs cells into expanded table form
[subT, new_subs] = expandSubsToTable(new_subs, f);

[newSubsMat,changeT] = updateFromSubTable(commonSubsMat, subT);
blankSubsMat = repmat('_',18,26);
[newSubsMat,changeT] = updateFromSubTable(blankSubsMat, subT);

% % Build the word subs table
% nsnp_start = cellfun(@(C) C{1}, subs);
% plain = cellfun(@(C) C{2}, subs, 'UniformOutput',false);
% ciph = cellfun(@(c) c{3}, subs, 'UniformOutput',false);
% wsubT = table(nsnp_start, plain, ciph);
% 
%subsMat = buildSubsMat(wsubT, f);
% Build the translated versions
f18 = build_oriented_frame(ospl, step_boundaries, 18);
tr2 = applySubsMat(newSubsMat, f);

% Show translated orig
%tr.oslash
%tr2.interlaced
%displayTranslationFigure(f,tr2);
changeT = sortrows(changeT,[1,2]);
displayTranslationFigureWithGold(f18,tr2,goldo,101)

%% Try a 36 alphabet version
% It seems like there are multiple cipher letters which are substituted with
% multiple plain letters.  It also seems that the substitutions are almost
% always 2 regular alphabet positions apart. This could correspond to a
% translation shift of the same cipher in the enciphering grid between
% alphabets which are 18 alphabets apart. Now that there are large chunks
% of plaintext available, I want to try to use them to construct the 36
% cipher alphabet version.  
toLettIdx = @(c) double(lower(c))-96; % convert letter to place in alphabet (a to 1 , b to 2, etc)

% In order to do this, we need to reconstruct the frame
[f36, c36] = build_oriented_frame(ospl, step_boundaries, 36);
% Build subT36
[subT36, new_subs] = expandSubsToTable(new_subs, f36);
% And try inverting the 36 alphabet version to look at the shifts that way

% Build subsMat36 and inverse directly from subT
subsMat36 = repmat('_',36,26); % plain letter is at (alph_num, ciph_lett_idx)
invSubsMat36 = repmat('_',36,26); % cipher letter is at (alph_num, plain_lett_idx)
for row_idx = 1:length(subT36.alph_num)
    nsnp_idx = subT36.nsnp_idx(row_idx);
    alph_num = subT36.alph_num(row_idx);
    ciph_lett = subT36.ciph_lett(row_idx);
    ciph_lett_idx = toLettIdx(ciph_lett); % ascii code to letter index for lowercase letters
    plain_lett = subT36.plain_lett(row_idx);
    plain_lett_idx = toLettIdx(plain_lett);
    % Check if overwriting
    existing_plain = subsMat36(alph_num,ciph_lett_idx);
    if existing_plain~='_' && existing_plain~=plain_lett
        % Overwriting!!
        fprintf('Diff plain!! nsnp:%i, Alphabet #%i, cipher "%s", plain "%s" to "%s"\n',nsnp_idx,alph_num, ciph_lett, existing_plain, plain_lett);
    end
    existing_ciph = invSubsMat36(alph_num, plain_lett_idx);
    if existing_ciph~='_' && existing_ciph~=ciph_lett
        fprintf('Diff ciph!! nsnp:%i, Alphabet #%i, plain "%s", cipher "%s" to "%s"\n',nsnp_idx,alph_num, plain_lett, existing_ciph, ciph_lett);
    end 
        
    subsMat36(alph_num, ciph_lett_idx) = plain_lett;
    invSubsMat36(alph_num, plain_lett_idx) = ciph_lett;
end
   
% This does not solve the problem, there are many instances of overwriting
% in both the forward and reverse directions. 

%% Build gold standard plaintext
% For gold standard plaintext, it doesn't matter what frame is used because
% alphabet numbers are not used for the gold standard, just nsnp_idxs,
% which are independent of frame
[subT36, new_subs] = expandSubsToTable(new_subs, f36);

goldn = repmat('_',size(f.nsnpl));
goldn(subT36.nsnp_idx) = subT36.plain_lett;
goldo = f.orig;
goldo(f.nsnp_idxs_in_orig_frame>0) = goldn;
upper_mask = f.orig~=lower(f.orig);
goldo(upper_mask) = upper(goldo(upper_mask));
goldo
    
%% Build a layered subsMat (try 18 and 36 and 72)
% OK, based on the gold standard text, lets build a 3D subsMat, where,
% instead of replacing a character, we add it to the 3rd dimension.  Each
% time we add a layer, the rest of the characters should be spaces (not
% underscores). Luckily, this is almost the default behavior (uses
% char(0), not ' ' which is char(32), but I think this will be OK).

% 18 alphabet version
f18 = build_oriented_frame(ospl, step_boundaries, 18);
[subT18] = expandSubsToTable(new_subs, f18);
[subsMat3d_18, invMat3d_18, nsnpIdxSubs3d_18, nsnpIdxInv3d_18] = build3dSubs(subT18,18);
tr3d_18 = apply3dSubsMat(subsMat3d_18, f18);

% 36 alphabet version
[subsMat3d_36, invMat3d_36, nsnpIdxSubs3d_36, nsnpIdxInv3d_36] = build3dSubs(subT36,36);
tr3d_36 = apply3dSubsMat(subsMat3d_36, f36);
%tr3d_36.orig3d

% 72 alphabet version
f72 = build_oriented_frame(ospl, step_boundaries, 72);
[subT72] = expandSubsToTable(new_subs, f72);
[subsMat3d_72, invMat3d_72, nsnpIdxSubs3d_72, nsnpIdxInv3d_72] = build3dSubs(subT72,72);
tr3d_72 = apply3dSubsMat(subsMat3d_72, f72);

tr3d_72.orig3d
%% 
displayTranslationFigureWithGold(f18,tr2,goldo,101)

%% Thoughts exploring the layered subsMats
% It appears that all of these frames (18, 36, 72), there are multiple
% layers to both the subMat and the invMat.  This means that for a given
% plaintext letter, there are usually two ciphertext letters which have
% been used to encipher it.  Also, for a given ciphertext letter, there
% are usually two plaintext letters it could represent. In general, I
% would assume that there should be a way to make the enciphering and
% deciphering unambiguous.  That is, there should be some sort of rule
% which determines which of the cipher letter options is used for a
% particular plaintext location. A simple rule would be alternating.
% Another option could be that if the same letter in the same alphabet
% number is about to be enciphered again, then add or subtract two
% alphabet locations (this is based on the fact that the plain letter
% options seem to very often be two places apart in the plain alphabet; for
% example, cipher letter 't' in alphabet 1 (for 18 alphs) means either
% plain 'd' or plain 'f'. 'd' and 'f' are two places apart ).
% Once I noticed this, I realized that I could often figure out the
% plaintext even when an incorrect plain letter was substituted in.  To
% get the proper plaintext, I was allowed to read the inserted letter +2
% or -2 places in the alphabet. thus ciphertext 'ybf' translated to 'tfe'
% could be read as 'the' because 'f' was two places away from 'h'.
% What would this mean about the enciphering process?  It would mean that
% cipher 'b' in that alphabet must sometimes represent 'h' and sometimes
% represent 'f'.
% plain 'e'

% Moving boundary 2 between 756 and 774 moves it to either side of gold 
% 'experience' at 762, but somehow, it seems to work out either way. 
% cipher: flmuqyjvdy either way
% With boundary at 756 (before), the initial alphabet is 14, where cipher
% 'f' is plain 'e'
% With boundary at 774 (after), the initaial alphabet is 10, where cipher
% 'f' is also plain 'e'

%% To expolore and display shifts in plain translations
% Perhaps the first encipherment step is typically to add or subtract 2
% letters to the plaintext letter, then encipher the new plaintext letter.
% The task then is to determine when you add and when you subtract. As a
% first attempt at this, I'll consider the first plaintext translation as
% the "base" one and take a look for any patterns in the ciphertext of the
% shifts. 
fir = double(subsMat3d_18(:,:,1));
sec = double(subsMat3d_18(:,:,2));
fir(fir==95) = 0;% underscores in the first layer should be considered the same as 0s in the second
chg = sec-fir;
chg(sec==0) = 0; % zeros in the second later mean we only saw one translation for this letter
chg(chg==24) = -2;
chg(chg==-24) = 2;
chg(chg==-22) = 4;

% Results: there are 
% sum(chg(:)==2) % = 165 shifts forward 2 letters
%sum(chg(:)==-2) % = 119 shifts backward 2 letters
%sum(chg(:)==4) % = 20 shifts forward 4 letters
%sum(chg(:)==-4) % = 0 shifts backward 4 letters
%sum(chg(:)==6) % = 1 shift forward 6 letters
%sum(chg(:)==-6) % = 1 shift backward 6 letters
% String representation
schg = char(zeros(size(fir)));
schg(chg==2)='+';
schg(chg==-2) = '-';
schg(chg==4) = '4';
schg(chg==-4) = '=';
schg(chg==6) = '6';
schg(chg==-6) = '^';

%% 12 cycle in cipher alphabets
% What can we make of the fact that the cipher alphabets seem to have a
% periodicity of 12 within them?  That is, if the plaintext letter 'e' is
% represented by a ciphertext letter, it is also likely to be represented
% by the ciphertext letter 12 letters away (for example cipher 'a' and
% cipher 'n'). This leads to striking almost repeats when looking at the
% cipher alphabet, where we often see somthing which looks like 'itenal'
% for example.  This seems problematic, though, as there are 26 plain letters to
% represent and only 12 spaces to represent them (if there were exact
% repeats).  This could be solved if each cipher letter represented two
% plain letters, but then we need disambiguation rules.
% What kind of encipherment scheme could produce 13 cycle repeats in the
% cipher alphabets??
% That would require plain letter number P to be represented by both cipher
% letter number C and cipher letter number C+13, for all plain letters.   
'abcdefghijklmn';
'mnopqrstuvwxyz'; %+12
% Actually, this is kind of great, because 12 is not exactly half, so one
% gap will be 12, but the other way will be 14, so we can tell which is
% which!
has_12shift = repmat(' ',size(subsMat3d_18(:,:,1)));
for row = 1:18
    for col = 1:26
        if col<=14
            recip_col = col+12;
        else
            recip_col = col-14;
        end
        letts = squeeze(subsMat3d_18(row,col,:));
        letts = letts(letts~=char(0) & letts~='~');
        other_letts = squeeze(subsMat3d_18(row,recip_col,:));
        other_letts = other_letts(other_letts~=char(0) & other_letts~='_');
        overlap = intersect(letts,other_letts);
        if length(overlap)>0
            has_12shift(row,col) = '+';
        else
            has_12shift(row,col) = '_';
        end
    end
end
%%
has_14shift = repmat(' ',size(subsMat3d_18(:,:,1)));
for row = 1:18
    for col = 1:26
        if col<=12
            recip_col = col+14;
        else
            recip_col = col-12;
        end
        letts = squeeze(subsMat3d_18(row,col,:));
        letts = letts(letts~=char(0) & letts~='~');
        other_letts = squeeze(subsMat3d_18(row,recip_col,:));
        other_letts = other_letts(other_letts~=char(0) & other_letts~='_');
        overlap = intersect(letts,other_letts);
        if length(overlap)>0
            has_14shift(row,col) = '+';
        else
            has_14shift(row,col) = '_';
        end
    end
end
%%
has_16shift = repmat(' ',size(subsMat3d_18(:,:,1)));
for row = 1:18
    for col = 1:26
        if col<=26-16
            recip_col = col+16;
        else
            recip_col = col-10;
        end
        letts = squeeze(subsMat3d_18(row,col,:));
        letts = letts(letts~=char(0) & letts~='~');
        other_letts = squeeze(subsMat3d_18(row,recip_col,:));
        other_letts = other_letts(other_letts~=char(0) & other_letts~='_');
        overlap = intersect(letts,other_letts);
        if length(overlap)>0
            has_16shift(row,col) = '+';
        else
            has_16shift(row,col) = '_';
        end
    end
end
%%
has_2shift = repmat(' ',size(subsMat3d_18(:,:,1)));
for row = 1:18
    for col = 1:26
        if col<=24
            recip_col = col+2;
        else
            recip_col = col-24;
        end
        letts = squeeze(subsMat3d_18(row,col,:));
        letts = letts(letts~=char(0) & letts~='~');
        other_letts = squeeze(subsMat3d_18(row,recip_col,:));
        other_letts = other_letts(other_letts~=char(0) & other_letts~='_');
        overlap = intersect(letts,other_letts);
        if length(overlap)>0
            has_2shift(row,col) = '+';
        else
            has_2shift(row,col) = '_';
        end
    end
end

% OK, so this is weird... Almost every cipher alphabet has the same
% plaintext letter appear both 12 and 14 cipher alphabet letters later, but
% not a single one appears 13 cipher alphabet letters later, and only two
% appear 16 cipher alphabet letters later (and they could potentially be
% related to poor boundary placement).  Not a single one has an 18 shift
% either. 

% What does this mean??  A plaintext letter in a certain cipher alphabet
% is almost always also represented by the cipher letter +12 and +14
% letters away.  However, this is not self-consistent... while the original
% letter is +14 from the first repeat and +12 from the second, this implies
% that +12 from the first repeat and + 14 from the second (which are -2 and
% +2 from the original, respectively) should also be repeats. Apply this a
% few times, and pretty soon EVERY second cipher character should represent
% that plain letter. 

% Interestingly, I do notice that the parity of the letters seems to be
% conserved in some way.  For example, in cipher alphabet 1, all odd plain
% letters are represented by odd cipher letters. Taking a look at this, it
% checks out! and every cipher alphabet alternates the parity.  The one
% exception I found actually led to the identification of a typo, which,
% when fixed, meant that there were NO contradictions to the parity rule in
% the first layer of the subsMat3d_18 matrix. 
parity = mod(int8(subsMat3d_18(:,:,1)),2)
parity(subsMat3d_18=='_') = 5 % these neither confirm nor deny...
figure; imagesc(parity)
% Should check the other layers, but I think this is going to work...
% Not sure what it means yet. 

% Note that all the vowels have the same parity.

%% I think it's time to start taking a look in ori to start to see
% patterns there. 
tori = repmat(' ', size(f18.ori));
tori(f18.nsnp_idxs_in_ori_frame>0) = goldo(f18.orig_idxs_in_ori_frame(f18.orig_idxs_in_ori_frame>0));
tori = lower(tori); 

e_ori = repmat('-', size(tori));
e_ori(tori=='e') = f18.ori(tori=='e');
e_ori(tori==' ') = '|';

[e_ori, e_odd] = makeLettOri(tori, f18, 'e');
[t_ori, t_odd] = makeLettOri(tori, f18, 't');
[a_ori, a_odd] = makeLettOri(tori, f18, 'a');
eta_odd = repmat(' ',size(tori));
eta_odd(a_odd=='#' | e_odd=='#' | t_odd=='#') = '#';
eta_odd(e_odd=='|') = '|';
eta_odd(a_odd=='o' | e_odd=='o' | t_odd=='o') = 'o';
[i_ori, i_odd] = makeLettOri(tori, f18, 'i');
[o_ori, o_odd] = makeLettOri(tori, f18, 'o');
[n_ori, n_odd] = makeLettOri(tori, f18, 'n');
eta_odd(e_odd=='|') = '|';
eta_odd(a_odd=='#' | e_odd=='#' | t_odd=='#' | i_odd=='#' | o_odd=='#' | n_odd=='#') = '#';
eta_odd(a_odd=='o' | e_odd=='o' | t_odd=='o' | i_odd=='o' | o_odd=='o' | n_odd=='o') = 'o'

% Playing with this suggests that very often, moving the boundary doesn't
% hurt good letters (while it might fix oddities).  This seems weird.  For
% a plaintext 'n' at nsnp 612, if the 

% If boundary 2 is moved between 613 and 703:
% * Alphabet 14 changes which letters it thinks are odd
% * nsnsp 613 is odd in 613 (where it is alphabet 9) and good in 703 (alph 5), but 
% * nsnp 614 and 615 are good in both 613 and 703 (10,11 vs 6,7)
% In gold, 613-615 = 'ean' in 'pine and'
% In cipher nsnp 613-615 = 'yjf'.
% In alphabet 9, cipher 'y' is plain... well, that depends on how we
% assemble the subsMat.  The 3D version is guaranteed to have the desired
% plain letter as an option, because we construct it from the gold text including 
% at this location.
% However, we're guessing there's an anomaly because we want to set things
% up so that so that each ciphertext has no more than two plain values, and
% we see three values... 

%% Four alphabets apart
% cipher 'f' in alphabet 7 means plain 'n' or 'l', whereas in alphabet 11
% it means 'p' or 'n'.  These overlap at 'n', so that means that a boundary
% shift crossing here but regarding a plaintext 'n', could look good in
% either one. I think this is the partial explanation for why the
% boundaries are hard to pin down exactly. 

% This pattern is not only for cipher 'f', it is also true for cipher 'g',
% which means 'w' or 'y' in 11 and 'y' and 'a' in 6. Can we use this
% pattern to fill out the whole alphabets?  Alphabet 6 has the letter two
% after the shared letter, whereas Alphabet 11 has the letter two before
% the shared letter. 
%
% C: abcdefghijklmnopqrstuvwxyz
% 7:    t pa     o kvgrc  lufq
% S:  v r ny     m itepa  hsdo 
%11:  t p lw       grcny  f bm

% consensus: 'kvgrcnylufqbm itepalwhsdo ' (missing j and z and x, and has two L's)
% filled: 'kvgrcnyjufqbmxitepalwhsdoz'
% This seems pretty promising... let's check out alphabets 15 and 3
consensus_alphabet = 'epalwhsdozkvgrcnyjufqbmxit';
ca2 = [consensus_alphabet, consensus_alphabet];
ca = consensus_alphabet;

cipher_alph_starts = [];
cipher_alph_starts(3,:) = [find(ca=='m'),find(ca=='o')];
cipher_alph_starts(7,:) = [find(ca=='k'), find(ca=='m')];
cipher_alph_starts(11,:) = [find(ca=='i'), find(ca=='k')];
cipher_alph_starts(15,:) = [find(ca=='g'), find(ca=='i')];
cipher_alph_starts(1,:) = [find(ca=='c'), find(ca=='e')];
cipher_alph_starts(5,:) = [find(ca=='a'), find(ca=='c')];
cipher_alph_starts(9,:) = [find(ca=='w'), find(ca=='y')];
cipher_alph_starts(13,:) = [find(ca=='u'), find(ca=='w')];
cipher_alph_starts(17,:) = [find(ca=='q'), find(ca=='s')];
cipher_alph_starts(4,:) = [find(ca=='t'), find(ca=='v')];
cipher_alph_starts(8,:) = [find(ca=='r'), find(ca=='t')];
cipher_alph_starts(12,:) = [find(ca=='p'), find(ca=='r')];
cipher_alph_starts(16,:) = [find(ca=='l'), find(ca=='n')];
cipher_alph_starts(2,:) = [find(ca=='h'), find(ca=='j')];
cipher_alph_starts(6,:) = [find(ca=='f'), find(ca=='h')];
cipher_alph_starts(10,:) = [find(ca=='d'), find(ca=='f')];
cipher_alph_starts(14,:) = [find(ca=='b'), find(ca=='d')];
cipher_alph_starts(18,:) = [find(ca=='x'), find(ca=='z')];



% Interesting note: 
% the looping consensus alphabet can be constructed by the following procedure:
% * start with an initial plain letter in the first position of the cipher
% alphabet
% * figure out the plain letter two places earlier in the regular alphabet
% (e.g. if the initial plain letter were 'e', the next plain letter would
% be 'c'
% * place this next plain letter 14 places later than the initial letter in
% the cipher alphabet.
% * find the next plain letter two places earlier in than the most recent
% plain letter (i.e. if the first letter was 'e' and the second was 'c',
% next would be 'a')
% * place this letter 14 places later in the cipher alphabet (mod 26 of
% course; since we've gone 2*14 so far, the next place will be two later
% than the initial position, so for cipher c). 
% * repeat until the next letter to place would be the initial plain
% letter. You will have filled in all of the odd places in the cipher
% alphabet. 
% * to fill in the rest, start with the plain letter 11 places later in the
% regular alphabet than the initial plain letter...
% The above procedure was inspired by the finding that a shifted copy of
% the cipher alphabet by 14 places puts letters which are 2 apart in the
% same locations (i.e. 'c' and 'e') and this is oddly true of ALL the
% letters. 

% ACTUALLY, you can just do that for the whole alphabet... the next plain
% letter in the cipher alphabet is 11 letters later in the regular alphabet

% Here's the one for cipher alphabet 1, starting with plain 'e'
a = mod(5+11*(0:25),26);
a(a==0) = 26;
alph(a)

%% OK, with full knowledge of the two cipher alphabets for each cipher alphabet
% We should be able to nail down EXACTLY where there are problems with the
% boundaries. (and then hopefully figure out the rules governing whether
% you use the earlier or later possible plaintext values)

% In fact, we should be able to construct an "earlier" and "later" subsMat
% and apply each to figure out which is correct in every place in the text.

% Another approach is that we should be able to figure out, for every gold
% letter, which cipher alphabets are possible for it, and that should have
% a maximum of two possibilities. 
% How would this work? We have a gold letter paired with a cipher letter.
% That pairing identifies exactly which consensus alphabet start was used,
% and each consensus alphabet start is identified with either one or two
% cipher alphabet numbers (only two if it is one of the  shared starts)

ciph_alph_starts = [
    'ce';
    'hj';
    'oq';
    'tv';
    'ac';
    'fh';
    'km';
    'rt';
    'wy';
    'df'; %10
    'ik';
    'pr';
    'uw';
    'bd';
    'gi';
    'ln';
    'qs';
    'xz'];
% I want to invert this so that I can look up the possible alphabet
% number(s) by the start letter. (done in findAlphNumsFromStartLett)

% Now, let's construct a the set of 
deciphStarts_nsnp = findFirstGoldLett(goldn, f18.nsnpl);
deciphStarts_nsnp(goldn=='_') = '_';
% For each first gold lett, I'd like to know which alphabet number(s) are
% possible.  This could be a vector, 

alphNums2 = findAlphNumsFromStartLett(deciphStarts_nsnp(1:3816), ciph_alph_starts);
alphNums2(:,goldn=='_') = -1;

% I want to wrap these at 18, but still interlace them.
r1 = reshape(alphNums2(1,:),18,[])';
r2 = reshape(alphNums2(2,:),18,[])';
r3 = zeros(size(r1,1)*3, size(r1,2));
for idx = 1:size(r1,1)
    r3(3*idx-2,:) = r1(idx,:);
    r3(3*idx-1,:) = r2(idx,:);
    r3(3*idx,:) = -idx;
end

%% This is fascinating
s18 = reshape(deciphStarts_nsnp(1:3816),18,[])'
e = repmat('-',size(s18));
e(s18=='_') = '_';
e(s18=='e') = 'e';
e(s18=='a') = 'a'

% s18 looks like slowly mutating repetition with a short cycle of 18, but
% usually 1-2 mutations per short cycle. Mutations look like they are
% essentially all -2 plain letter locations (the first is w->u). 

% The next mutation is typically 13 away, but is sometimes 31 away.
% 31 means that a whole row is repeated with no mutation from the previous
% one, then 13 spacing resumes (13+18=31)


%% What does the inverse of the consensus alphabet look like?
% The inverse of the consensus alphabet looks like 
invertAlph(makeC11('e'))
% 'cvohatmfyrkdwpibungzslexqj'

% This alphabet can be constructed by +19 (or -7) mod 26 cycling

%% What does the sequence of enciphering alphabets look like?
numRows = 370;
enciph = repmat(' ',numRows, 26);
for row = 1:numRows
    enciph(row,:) = invertAlph(makeC11(deciphStarts_nsnp(row)));
end

% Shifts look like +9s and -3s
dd = diff(double(enciph(:,1)));
dd(dd==-17)=9;
dd(dd==23)=-3;

% So the next letter is (always?) enciphered by an alphabet19 starting with
% the letter which is either +9 or -3 from the alphabet19 used for the
% current letter. However, it isn't clear how you decide whether it is +9
% or -3. It is +9 ~62% of the time, and -3 ~38% of the time
sum(dd==9)/length(dd);
sum(dd==-3)/length(dd);

reshape(dd(1:360),18,[]);

%% Let's find the full sequence of enciphering alphabet first letters
enciphStarts = repmat('_', size(goldn));
for idx = 1:length(goldn)
    enciphStarts(idx) = firstEnciphFromFirstGold(deciphStarts_nsnp(idx));
end
dd = diff(double(enciphStarts));
dd(dd==-17)=9;
dd(dd==23)=-3;

% After checking areas where diff was not -3 or +9 and fixing those, ALL
% diffs are either -3 or +9!!
%% There are repetitions of 18 alphabet sequences
strfind(deciphStarts_nsnp, deciphStarts_nsnp(1:18))
diff(strfind(deciphStarts_nsnp, deciphStarts_nsnp(1:18)))
% 374   374   374   392   374   374   374   392   374   374
diff(strfind(deciphStarts_nsnp, deciphStarts_nsnp(19:36)))
% 374   374   374    18   374   374   374   374    18   374   374   374

%% Should also now be easily possible to fill in the last remaining gap 
% in the gold text
gapFirstGoldLetts = 'ejqvchmtydkpwb'; % or the 'c' might be 'a'
gap_ciphtext = f18.nsnpl(goldn=='_');
for idx = 1:length(gap_ciphtext)
    ciph_alph = makeC11(gapFirstGoldLetts(idx));
    gap_plain(1,idx) = ciph_alph(toLettIdx(gap_ciphtext(idx)));
end
    
%% Address error regions
err_reg = 1048:1060; 
error_ciphtext = f18.nsnpl(err_reg);
err_deciphStarts = deciphStarts_nsnp(err_reg);
err_deciphStarts(2:3) = 'sz'; %'uz' % could be 'sz'

for idx = 1:length(error_ciphtext)
    ciph_alph = makeC11(err_deciphStarts(idx));
    err_plain(1,idx) = ciph_alph(toLettIdx(error_ciphtext(idx)));
end
%%
err_reg2 = 2935:2950; % just 2938, probably
error_ciphtext2 = f18.nsnpl(err_reg2);
err_deciphStarts2 = deciphStarts_nsnp(err_reg2);
err_deciphStarts2(4) = 'z';

for idx = 1:length(error_ciphtext2)
    ciph_alph = makeC11(err_deciphStarts2(idx));
    err_plain2(1,idx) = ciph_alph(toLettIdx(error_ciphtext2(idx)));
end

% In each case, the pattern was correct, and my original gold translation
% was incorrect (I used 'in' when correct was 'at', and I misspelled
% 'expression')

%% Going back to repeat patterns present in the deciphStarts
% A sequence of 18 letters repeats 374 or 392 (374+18) or 18 letters apart.
diff(strfind(deciphStarts_nsnp, deciphStarts_nsnp(1:18)))
%374   374   374   392   374   374   374   392   374   374
diff(strfind(deciphStarts_nsnp, deciphStarts_nsnp(12+(1:18)))) 
% starting at 13 same as starting at 1, however, starting at 14 is
% different:
diff(strfind(deciphStarts_nsnp, deciphStarts_nsnp(13+(1:18))))
% 374   374   374    18   374   374   374   374    18   374   374   374
% 18 means that exact sequence is repeated twice in a row

% Maximally, the sequence from 1:121 matches the sequence of 121 starting
% at 375.  However! The 122nd deciphStart letter differs from the 122nd
% after 375. That was unexpected.  121 = 11*11, which likely matters.
n=121;
all(deciphStarts_nsnp(1:n)==deciphStarts_nsnp(375:(375+n-1))) % True
n=122;
all(deciphStarts_nsnp(1:n)==deciphStarts_nsnp(375:(375+n-1))) % False

%% Exploring the patterns in d18
ddd = diff(double(deciphStarts_nsnp));
ddd(ddd==-21)=5;
ddd(ddd==-19)=7;
% ddd is equivalent to dd; everywhere dd has 9, ddd has 5, and everywhere
% dd has -3, ddd has 7. I want to find ways to visualize the pattern

f30 = figure(30); clf
period = 18; 
d18 = reshape(ddd(1:(period*floor(length(ddd)/period))), period,[]);
% Make this into a meaningful image
im = d18; % d18 is all 5s and 7s
% I want im to have information about whether an entry is a 5 or a 7, and
% about the length of the run it is part of.  How about, if it is a 5 and
% is part of a run of length 8, then it's pixel value should be 58. 

% We can find starts and stops of runs by finding where diff is not zero.
% If diff(idx)==2, then idx has 5 and idx+1 has 7; if diff(idx)==-2, then
% idx has 7 and idx+1 has 5. 
edges = diff(im,1,2);
for row_idx = 1:size(im,1)
    to7 = edges(row_idx,:)==2;
    to5 = edges(row_idx,:)==-2;
    start5run = 1+find(to5);
    end5run = find(to7);
    start7run = 1+find(to7);
    end7run = find(to5);
    if im(row_idx,1)==5
        start5run = [1,start5run];
    else
        start7run = [1,start7run];
    end
    if im(row_idx,end)==5
        end5run = [end5run, size(im,2)];
    else
        end7run = [end7run, size(im,2)];
    end
    % Loop over starts and ends to determine lengths and fill in values
    for idx5 = 1:length(start5run)
        len = end5run(idx5)-start5run(idx5)+1;
        val = 0.5+len;
        im(row_idx, start5run(idx5):end5run(idx5)) = val;
    end
    for idx7 = 1:length(start7run)
        len = end7run(idx7)-start7run(idx7)+1;
        val = 0.7+len;
        im(row_idx, start7run(idx7):end7run(idx7)) = val;
    end
end

f30 = figure(30); clf;
imagesc(im);
% This figure might be helpful for discerning patterns, but it also might
% obscure some...

%% Repeated enciphAlph (or, equivalently, deciphAlph) sequences of 18
immediateRepeats = [];
for idx = 1:(length(enciphStarts)-36)
    if enciphStarts(idx:(idx+17))==enciphStarts(idx+18:idx+35)
        immediateRepeats(end+1,1) = idx;
    end
end    

% Are these longer than expected?  Maybe not, because maybe it's just the
% 18 repeat + the normal mutation spacing (check this!).

% Excitingly, they seem to be predictable!  Alternating 149 and 162 between
% starts, but strictly alternating, so predictable. These are only 13
% apart, so maybe even there's some way it works out that these are the
% same spacing (for example, where they fall in the mutation schedule?).
% Even if not, it's not a problem if we know exactly where the next one
% goes.
dIR = diff(immediateRepeats)
dIR(dIR~=1)

% I think it's time to go back to examining the pattern in "mutations"
%% "Mutation" analysis
% THis time in enciph space (should be equivalent to deciph space, I'm just
% going back to it)
period = 18; 
enci18 = reshape(enciphStarts(1:(period*floor(length(enciphStarts)/period))), period,[])'

% Note that mut18 marks mutations on the row before they happen...
mut18 = diff(double(enci18));
mut18(mut18==-14)=12;
% All 12 or zero now
mutSpacing = diff(find(a2v(mut18')==12));
% All 13 or 31 (13+18)
% IF we could predict where the 31's are, then we are DONE with pinning
% things down
diff(find(mutSpacing==31))
% Yes!! This strictly alternates between 12 and 11.
% Let's check the initial gap to see if that fits too
find(mutSpacing==31,1,'first') % =10, meaning that there are 9 mutations 
% with 13 spacing before the first 31 spaced mutation. 
% ACTUALLY, if we consider ther first row, there would be expected to be
% two mutations in it, at position 18 and position 5. We can't detect those
% because we don't know the row before to see what changed, but if we
% include those, then there are 11 mutations before the first 31, then 12
% more between first and second, and then 11 between the second and 3rd,
% etc.  

%% What does the encipherer need to know to set the pattern?
% * The initial 18 encipherment alphabet letters (before any mutations)
%   De-mutated: 'cliroxgdmvsbyhqnwt' (from 'cliraxgdmvsbyhqnwf') 
% * That the first mutation should happen at position 5
% * That mutations should happen every 13 enciphered letters after that,
% except that there should be periodic longer gaps of 31 (which is 13+18) between mutations.
% * The first longer gap should be the 11th mutation.
% * The next longer gap should be +12 mutations later, then the next after
% that should be +11 mutations later, etc, alternating sets of 10 or 11
% shorter mutation gaps between longer ones.
% * That mutations should change the encipherment alphabet letter by +12


%%
% I wonder if there might be just a set of progressive rules about how to
% encipher the next letter, rather than a set of rotating cipher alphabets.
% If so, I'm not sure how to figure out what it is.

%% 
% The first gold character which causes a layering in the subsMat3d_18 is
% at nsnp 101, conflicting with nsnp 11.  Both are cipher 'q' in alphabet
% 11, but at 11, it is plain 'e' in between and at 101 it is plain 'c' in
% continuing.  This is well before the first step boundary, so it can't be
% that things switch at step boundaries. If we look at the pattern for
% cipher 'q' substitution in alphabet 11, it is 'ece_eec_e_c_eee', which is
% not strictly alternating. The conflicts are 90 apart (5*18). I don't see
% an easy pattern in the nsnp_indices where either appears (e.g. 'e' if 

% What is the next conflict? Let's check a few more:
% Actually there is an earlier one which wasn't present yet before because
% I hadn't figured out the gold text for it yet.
% nsnp 80 conflicts with nsnp 26, both in alphabet 4. Cipher is 'd' in both
% places, but plain 'a' in 26 and plain 'y' at 80.  The pattern is:
% 'ayyy___a_', which is also not alternating.  Conflicts are 54 apart
% (3*18).

%% What if we tried with no step boundaries, would things be more confused



%% What if we tried to move everything into letter index math...
% Would things simplify at all?
% For encipherment:
%(plainLettIdx-1)*19 + startIdxFrom18
%
% Then mutation means this:
%(plainLettIdx-1)*19 + startIdxFrom18 + 12
% or equivalently
%((plainLettIdx-1)+2)*19 + startIdxFrom18 
% Note that this +2 is probably why plaintext letters look like +2 after
% mutation (but why -2? and why would it go back and forth?)

% offsets18 = toLettIdx('cliroxgdmvsbyhqnwt')

% Interestingly, even for long gap mutations, the sequence of starting
% letter differences preceding it are the same for 30 differences as every
% other mutation (notably, this is longer than the 13 character cycle).
% This means that there is a full 13 character difference sequence which would
% normally occur right before a mutation, but doesn't result in one??

%% Rethinking
% The sequence of starting letters can be thought of as a sequence of
% starting indices, and therefore, the differences can be thought of as
% additions and subtractions to this starting index.  These shifts are ALL
% equivalent to +9 or -3.  These shifts are generally in a 13 step cycle,
% which is sometimes altered.  If we were to determine the rules for this
% cycle and it's alterations, we could discard the idea of an 18 alphabet
% cycle.  Instead, the enciphered letter index would be given by 
% (plainLettIdx-1)*19 + currentOffset
% Where currentOffset starts at 3 and is modified by either +9 or -3 with
% every enciphered letter. 
% The starting sequence of shifts is:
% [9    -3     9     9    -3     9    -3     9     9    -3     9    -3     9]
% [+-++-+-++-+-+]
% This is repeated  several times, but then shifts every once in a while.
% Shifts can be characterized in a few different ways.  One way is that you
% jump forward 8 (or, equivalently, backwards 5) places in the cycle, and
% continue from there. Alternatively, the change can be characterized as
% swapping two adjacent numbers in the cycle. In this view, it is always a
% second 9 swapping with the following -3, and it is always the latter
% second 9 in the sequence -3 9 9 -3 9 9 -3, becoming -3 9 9 -3 9 -3 9.
% This has the effect of switching the first -3 9 9 to become the isolated
% -3 9 9, and the newly created -3 9 9 becoming the first in the pair of
% adjacent -399s.

% Where does 18 come from then???  The sum of this pattern after 18 places
% is 78, which is 26*3, which means that after 18 places, one returns to
% the same offset (equivalent to the same cipher alphabet). 
%% Where are offset shifts?
% First offset mutation is at 160, and then the diffs between them
% strictly alternate between 174 and 161 apart.  

% Really, this should be enough to make a new (simpler?) version of encipher.m
%% Created encipherM here
% Needed inputs for encipherment function are:
% offsetShiftCycle = [9, -3, 9, 9, -3, 9, -3, 9, 9, -3, 9, -3, 9]
% offsetMutationShift = 8
% initialOffset = 3
% plaintLettMultiplier = 19
% mutationGaps = [161,174];
% initialMutationCountdown = 160; % can probably figure out how to rewrite this to be 161

% Decipherment can actually be done with the same function, just by
% changing the offsetShiftCycle, initial offset, and multiplier
% offsetShiftCycle = [5,7,5,5,7,5,7,5,5,7,5,7,5] % 9 -> 5, -3 -> 7
% initialOffset = 5
% multiplier = 11

%% Creating decipher function from encipher parameters
% How are multipliers related?
% By finding how many steps apart in one alphabet a one-step difference is
% in the other.   For example, 19*11 mod 26 is 1, so 19 and 11 form a pair
% of multipliers; if 19 is used for enciphering, 11 should be used for
% deciphering. You can see this because if you use 19 for enciphering then
% for each one letter place change in plaintext alphabet means a 19 letter
% place change in the cipher alphabet. To invert that, we want to know what
% a one letter place change in the cipher alphabet change corresponds to in
% the plaintext alphabet, which means we want to know how many +19s we have
% to do to get to a net +1 change. 19*[1:26] mod 26 will give us the net
% effect, and we just need to look for the +1. For 19, this occurs at 11
% steps of size +19.  This is because 11*19 mod 26 = 1.  Note that because
% multipliction is commutative this also means that if the multiplier is
% +11, then it will take 19 steps to get a net +1 change.  
% This function will find the paired multiplier for any supplied
% multiplier, returning an empty row vector for non-allowed multipliers
% (which is anything which shares a factor with 26 (2 and 13 are the only
% factors))
mod26 = @(N) mod(N-1,26)+1;allowedMultipliers = [1,3,5,7,9,11,15,17,19,21,23,25];
findPairedMultiplier = @(multiplier) find(mod26([1:26]*multiplier)==1);

allowedMultipliers = [1,3,5,7,9,11,15,17,19,21,23,25]; 
% Note that 1 means no encipherment (plain==ciph), and 25 means a reversed
% order normal alphabet (a=z, b=y, c=x, etc). Multipliers > 26 are allowed,
% but behave the same as taking mod26 of them first. For example, a
% multiplier of 29 is the same as a multiplier of 3 (because 29 * N mod 26 
% is the same as 3 * N mod 26 (because (26 + 3) * N = 26*N + 3*N and 26*N
% mod 26 is zero.  

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
% There is probably a simple mapping.  We have already figured out that the
% number of steps in the derived alphabet which gets to a net +1 letter
% place was the key to finding the paired multiplier.  I suspect we want to
% do somthing similar here, but if the offsetShift is +9, then instead of
% looking for where the net shift is +1, let's look for where the shift is
% +9. Likewise, for shifts of -3 (=+23) let's look for where the shift is
% +23.
mod26 = @(N) mod(N-1,26)+1;
find(mod26([1:26]*19)==9) % =21, looking for 5
find(mod26([1:26]*19)==23)% =19, looking for 7
% Well, empirically, I see that the answers I want are 26 minus the answers
% I got...
% Maybe this would work?
%findPairedOffsetShift = @(offsetShift1,multiplier1) 26 - find([1:26]*multiplier1==mod26(offsetShift1));
% Actually, findPairedOffset should work at any point in the text, so we
% could also do the following
multiplier1 = 19;
offset1 = 3;
offsetShift1 = 9;
pairedOffsetFirst = findPairedOffset(multiplier1, offset1)
pairedOffsetSecond = findPairedOffset(multiplier1, offset1 + offsetShift1)
offsetShift2 = mod26(pairedOffsetSecond-pairedOffsetFirst)
% Though some algebra, this can be simplified down to 
findPairedOffsetShiftCycle = @(multiplier1, offsetShiftCycle1) mod26(findPairedMultiplier(multiplier1) .* (-offsetShiftCycle1));
% Wrote createEnciphDeciphPair.m here
offsetShiftCycle = [9, -3, 9, 9, -3, 9, -3, 9, 9, -3, 9, -3, 9];
[enciphFcn, deciphFcn] = createEnciphDeciphPair(19,3,offsetShiftCycle, 8, [161,174],160);

%% Variations %%
% It would be interesting to create variations on the encipherment scheme
% and evaluate them for their ability to create difficult to crack codes.
% One approach might be to look at and count repeated ciphertext snippets, check
% if they represent actual repeated plain text, and evaluate their spacings?

% Wrote autoAssess.m here with initial word analysis

%% What is the effect of changing initial offset? (suspect change symbols
% but not repeats)
% Confirmed
[defaultEnciphFcn, defaultDeciphFcn] =   createEnciphDeciphPair(19, 3, offsetShiftCycle, 8, [161,174], 160);
[enciphFcn4, deciphFcn4] = createEnciphDeciphPair(19, 4, offsetShiftCycle, 8, [161,174], 160);
rs = autoAssess(defaultEnciphFcn, goldo);
rs4 = autoAssess(enciphFcn4, goldo); 
% Same number of words and repeats, in all the same locations, just with
% differing cipher symbols

%% What if we use a more complex shift cycle?  
newOffsetShiftCycle = [offsetShiftCycle, 9]; % change length, add extra +9
[efOSC, dfOSC] = createEnciphDeciphPair(19, 3, newOffsetShiftCycle, 8, [161,174], 160);
rsOSC = autoAssess(efOSC, goldo)
% Autocorrelation patterning falls apart compared with original, which has
% a very strongly periodic autocorrelation. 
figure;
subplot(2,1,1)
plot(rs.autoCorrLags, rs.autoCorr);
ylabel('Fraction matching');
xlabel('Lag');
title('Default')
subplot(2,1,2)
plot(rsOSC.autoCorrLags, rsOSC.autoCorr);
title('offsetShiftCycle is 14 long with extra +9 added')
ylabel('Fraction matching')
xlabel('Lag')
%% What if we use more frequent (or absent) mutation schedule?
[efNoMut,dfNoMut] = createEnciphDeciphPair(19, 3, offsetShiftCycle, 0, 161000, 160);
rsNoMut = autoAssess(efNoMut,goldo)

%Interestingly, removing mutation doesn't have a huge effect.  It shortens
%the long cycle from ~374 to 338, makes it so that 338 repeats have
%autocorrelation of 1 (up from ~0.98), DECREASES number of repeated words
%by one to 56, but increases total number of repeats by 7 to 75.  Short and
%mid cycles are still apparently 18. 

%% Try the other cycle with no mutation...
[efNoMutOSC,dfNoMutOSC] = createEnciphDeciphPair(19, 3, newOffsetShiftCycle, 0, 161000, 160);
rsNoMutOSC = autoAssess(efNoMutOSC,goldo)

% Big effect here of removing mutation! Becomes perfectly regular with long
% cycle of 182. I see gaps of 18, 32, 14, all sort of intermixed within
% that 182 cycle. 

%% What if we try different multipliers?
[efMult3, dfMult3] = createEnciphDeciphPair(3, 3, offsetShiftCycle, 8, [161,174],160);
rsMult3 = autoAssess(efMult3, goldo)

% Changing the multiplier changes the alphabets, but not the starting
% letters.  This means that it is fully the offsetShiftCycle + mutations
% which lead to repetitions.  

%% How long until there is a FULL repeat?  (i.e. same sequence of cipher
% alphabets including mutations)

% Answer is 8711 for the default.  1:8710==8711:8711+8710-1
factor(8710) % = 2, 5, 13, 67
% I notice: 5 * 67 = 335 = 161+174 !
% 2 * 13 = 26 , but I would guess this has more to do with the cycle being
% 13 long than that there are 26 letters in the alphabet. 
% I guess I might have expected that the period would be (161+174)*13,
% because you might need 13 mutations in each long block to get back around
% to the starting point.  I'm not sure where the extra *2 comes from. 

% What is the short cycle? 
% this can be found by
% find(mod26(cumsum(repmat(offsetShiftCycle,1,5)))==26,1,'first')
% as long as the pattern generally repeats from there. This does not need
% to be the case in general, but is the case for the original
% offsetShiftCycle, I think because mutation leaves most columns alone?
% Still a bit confusing to me how well the 13 cycle leads to an 18 cycle.
% Why is mutation necessary? (i.e. does repetition show up too much without
% it?)

%% Quasi-periods based on offsetShiftCycles 
cm = []; 
repeatedNewOSC = repmat(newOffsetShiftCycle, 1, 10); 

for quasiperiod = 1:100 
    numberOfPlaces=[]; 
    for idx = 1:length(newOffsetShiftCycle) 
        % Find the number of places in the cycle where the sum of the next
        % quasiperiod offset shifts is a multiple of 26
        numberOfPlaces(idx) = sum(mod26(sum(losc(idx-1+(1:quasiperiod))))==26); 
    end
    cm(quasiperiod) = sum(numberOfPlaces); 
end
cm
% The values in cm are the number of places in the offsetShiftCycle where
% the sum of the next quasiperiod shifts turns out to be a multiple of 26,
% meaning the same cipher alphabet will be used in that place as in
% quasiperiod earlier or later places. 
% For example, cm(50) = 10 for the newOffsetShiftCycle, which means that
% 10 of the 14 places in the newOffsetShiftCycle will have use the same
% offset as the place 50 characters after them. That is, for a quasiperiod
% of 50 characters, 10/14 of places will use the same cipher alphabet. 

%% Deeper understanding?
% Where does the offset shift cycle come from?  Why 9 and 23 (or -3)? Why
% that pattern?  It seems hard to remember. 
% Why gaps of 161 and 174?  Why not consistent?  Why so long? Why not full
% cycles (i.e. multiple of 13)?


