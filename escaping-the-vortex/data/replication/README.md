# Replication data

Derived, pseudonymised data used in the article. They are produced by `R/08_export_data.R` from the cached results of `run_all.R`. No usernames, comment texts or titles are included. Contributor usernames in `fork_contributors.csv` are replaced by random IDs.

| File | Level | Used for |
|---|---|---|
| `panel_platform_month.csv` | Platform × month | H1, H2 (Tables 4, 5; Figure 2) |
| `coalitions_landmark14.csv` | Deliberation (thread), coalition measured in first 14 days | H3 (Table 6) |
| `coalitions_wholelife.csv` | Deliberation, coalition over the thread's whole life | Table S1 |
| `fork_contributors.csv` | Early fork contributor | H4 (Table 7; Figure 3) |
| `topic_labels.csv` | Topic (K = 30) | Topic labels |

## Key variables
**Panel:** `platform`, `month`; `stalemate` (share of the month's threads unresolved after 90 days), `polarisation` (Louvain modularity of the co-deliberation network), `closure_rate` (closure expressions per comment), `topic_entropy`; `rop` (latent-tie share), `rop_bridge` (bridging latent-tie share); `z_*` (within-platform z-scores); `vortex` (mean of the four z-scores); `role` (treated / fork / control), `shock` (licence-change date), `parent`.

**Coalitions:** `platform`, `number` (GitHub issue/PR number), `is_pr`, `created`, `closed`, `resolved` (1 = closed), `duration` (days, from the landmark in the landmark file); `size`, `parsimony`, `necessity` (maintainer present), `bypass`, `variety` (Blau index of affiliations), `variety_missing`, `known_org_share`.

**Fork contributors:** `fork`, `contributor_id`, `active_months` (months active out of `follow` = 18), `early_events`, `log_early`, `from_parent` (active in parent before the fork), `polar_origin` (H4a), `latent_origin` (H4b).
