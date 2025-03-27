# SciFundX: Decentralized Scientific Research Funding

SciFundX is a blockchain-based platform that revolutionizes how scientific research is funded, reviewed, and supported using decentralized technology.

## Overview

SciFundX addresses critical challenges in scientific research funding by creating a transparent, community-driven ecosystem where:

- **Scientists** can submit research proposals and receive funding directly from interested backers
- **Evaluators** can review and vote on the scientific merit of proposals
- **Backers** can financially support research they believe in and receive refunds if projects don't reach funding targets

## Key Features

- **Transparent Proposal Process**: Scientists submit detailed project proposals with clear funding targets
- **Peer Evaluation System**: Qualified evaluators review and vote on proposals to ensure scientific merit
- **Direct Funding Mechanism**: Once approved, projects enter an open funding stage where backers can contribute
- **Automatic Fund Distribution**: Successfully funded projects automatically receive funds when targets are met
- **Refund Safeguards**: Backers can request refunds for projects that don't reach funding targets or fail to launch

## How It Works

1. **Proposal Submission**: Scientists submit research proposals with title, description, and funding target
2. **Evaluation Phase**: Qualified evaluators review proposals and vote to approve or reject
3. **Funding Phase**: Approved proposals enter an open funding period where backers can contribute
4. **Project Execution**: Upon reaching funding targets, scientists can claim funds to conduct research
5. **Refund Mechanism**: If a project fails to meet its funding target by the deadline, backers can request refunds

## Contract Functions

### For Scientists

- `submit-project`: Submit a new research proposal
- `claim-funds`: Withdraw funds once a project is fully funded

### For Evaluators

- `evaluate-project`: Vote to approve or reject a proposal

### For Backers

- `back-project`: Contribute funds to an approved research project
- `request-refund`: Request a refund for eligible projects

### For Contract Administrators

- `update-project-status`: Update a project's status
- `add-evaluator`: Add a qualified evaluator
- `remove-evaluator`: Remove an evaluator
- `set-required-evaluations`: Set the number of evaluations required
- `set-funding-duration`: Set the funding period duration

## Getting Started

To interact with the SciFundX contract:

1. Clone this repository
2. Deploy the contract to a Stacks blockchain environment
3. Interact with the contract using a compatible wallet or development tools

## Technical Details

SciFundX is built on the Stacks blockchain using Clarity smart contracts, providing:

- Secure and transparent fund management
- Immutable record of proposals and evaluations
- Trustless execution of funding distribution

## Contributing

We welcome contributions to improve SciFundX! Please see our [Contributing Guidelines](CONTRIBUTING.md) for more information.


## Contact

For questions or feedback about SciFundX, please [open an issue](https://github.com/blessing-eb56/scifundx/issues).