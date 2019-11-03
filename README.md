# Capture PD Alerts as Trello Cards

This service is to capture new alerts for a team and create them as Trello cards in the team's Trello board

1. Create cards in team's triage list
2. Add labels which is configured in configuration file
3. Assign the cards to the PagerDuty On-Call user

## Deployment
This app is deployed as Fargate scheduled task with Shipper 2.0


### Configuration
This is under config directory

```yaml
trello:
  # Board name where Card
  board-name: The Castle

  # List where cards are created
  column-for-triage: Triage

  # Trello API does not publish users' email address so manual mapping
  # between okta email and trello id is required
  members:
    - "daniel,choi@amazonaws.com"
    - "ayseasa,ayse@face.com"

  # labels to be set
  labels-for-new-card:
    - unplanned
    - Custodianship

pagerduty:
  # Team(Schedule) name to monitor
  team-name: Group Technology - DS - Property Insights
```

### Environment variables should be set
which is under ./shipper/common.yml

```yaml
containers:
  app:
    image: "<(var.image)>"
    environment:
      TRELLO_API_URL:
      TRELLO_API_KEY:
      PAGERDUTY_API_URL:
      CONFIG_FILE_PATH:
      KMS_ENCRYPTED_TRELLO_API_TOKEN:
      KMS_ENCRYPTED_PAGERDUTY_API_TOKEN:
```

## Credit
1. Deployment script is from @alistair-burrowes 's this repo, https://git.realestate.com.au/alistair-burrowes/see-pr-bot
2. Learned how to use [Polysemy](https://github.com/polysemy-research/polysemy#readme) from this repo, https://git.realestate.com.au/alistair-burrowes/label-maker
3. This blogs are really good starting point for `Polysemy`
   - https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1
   - https://haskell-explained.gitlab.io/blog/posts/2019/07/31/polysemy-is-cool-part-2
