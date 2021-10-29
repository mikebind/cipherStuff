function subtext = subfunc1(substext,alphnum,letter,replacement)

sub = find(substext(:,alphnum)==letter);
substext(sub,alphnum) = replacement;
subtext = substext;
return



