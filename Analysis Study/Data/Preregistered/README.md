---
output:
  html_document: default
  pdf_document: default
---
## Data Description

The dataset is organized in a long format.

## Variables description

-   subject: Participant id
-   Gender: (Female; Male; Prefer not to say)
-   Age
-   Income
-   Education: Higher values corresppond to higher education
-   Country
-   Finantial Litaracy
-   condition:
    -   [MrAB](https://run.pavlovia.org/simonedambrogio/mmar-questions/#mrab): (Gain-gain VS gain; Gain-loss VS gain; Loss-gain VS loss; Loss-loss VS loss)
    -   [Game](https://run.pavlovia.org/simonedambrogio/mmar-questions/#game): (Friend; Stranger)
    -   [Drink](https://run.pavlovia.org/simonedambrogio/mmar-questions/#drink): (Grocery Store; Resort Hotel)
    -   [Jacket](https://run.pavlovia.org/simonedambrogio/mmar-questions/#jacket): (High; Low)
    -   [Play](https://run.pavlovia.org/simonedambrogio/mmar-questions/#play): (Ticket; Cash)
    -   [Gym](https://run.pavlovia.org/simonedambrogio/mmar-questions/#gym): (Per-session; Yearly)
    -   [Plane](https://run.pavlovia.org/simonedambrogio/mmar-questions/#plane): (Free; Purchased)
-   response:
    -   [MrAB](https://run.pavlovia.org/simonedambrogio/mmar-questions/#mrab): (1: "Mr. A"; 0: "Mr. B")
    -   [Game](https://run.pavlovia.org/simonedambrogio/mmar-questions/#game): (1: price \>= market_value; 0: price \< market_value)
    -   [Drink](https://run.pavlovia.org/simonedambrogio/mmar-questions/#drink): (z scored log(price))
    -   [Jacket](https://run.pavlovia.org/simonedambrogio/mmar-questions/#jacket): (1: "Yes"; 0: "No")
    -   [Play](https://run.pavlovia.org/simonedambrogio/mmar-questions/#play): (1: "Yes"; 0: "No")
    -   [Gym](https://run.pavlovia.org/simonedambrogio/mmar-questions/#gym): (z scored response)
    -   [Plane](https://run.pavlovia.org/simonedambrogio/mmar-questions/#plane): (1: "Pay your friend"; 0: otherwise)
