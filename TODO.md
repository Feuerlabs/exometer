# Improvements

- report plugin processes are currently manually monitored by exometer_report, this should be changed to a more robust appraoch. Could use simple_one_for_one, which lacks restarts. Can't use other normal supervision strategies without some changes as we don't want external systems to trigger a supervision tree collapse.

# Bugs

- fix all dialyzer errors
- type specs for SNMP stuff
- subscription parameter extra needs to be sent to manager as part of inform

# Misc

- use proper module organization in all modules, e.g. exports, includes, external API, internal API
- update documentation
