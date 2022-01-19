# Analysis: Number of percentages on module documentation
We have conducted a deeper analysis of the number of percentages used for module documentation. This is meant to guide our recommendation of the `%%% % @format` module pragma.

From the empirical results below, it shows that while there is no explicit consensus, most projects prefer using `%%%` for module documentation, except for OTP which strongly favors `%%`.

### Methodology
We first isolated the comments located above the `-module` attribute and counted the number of blocks (as opposed to single lines)
for each number of percentage, this avoids skewing the results towards lengthy multi-line comment blocks. We also removed all `%% @format` headings to give a better representation of usage.
We have then outputted as a percentage of total blocks the number of percentages used by each repository.

### Results

```
OTP
%: 0.7%
%%: 91.5%
%%%: 7.8%

ejabberd
%: 0.0%
%%: 16.9%
%%%: 83.1%

MongooseIM
%: 0.2%
%%: 40.1%
%%%: 59.7%

Inaka
%: 8.3%
%%: 23.4%
%%%: 68.3%

kazoo
%: 0.0%
%%: 0.0%
%%%: 100.0%

WhatsApp
%: 17.4%
%%: 15.6%
%%%: 67.0%
```
Source: [./module_comments.sh](./module_comments.sh)
