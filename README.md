# Option Pricing Simulator

A Haskell-based financial option pricing service that implements multiple pricing methods including Monte Carlo simulation, Black-Scholes, and Finite Difference methods for European-style options.

## Features

- RESTful API for option pricing
- Multiple pricing methods:
  - Monte Carlo simulation
  - Black-Scholes analytical solution
  - Finite Difference (Crank-Nicolson) method
- Support for both Call and Put options
- Configurable parameters for all pricing methods

## Prerequisites

To build and run this project, you need:

1. Install GHC (Glasgow Haskell Compiler) and Stack:
   ```bash
   # For Ubuntu/Debian
   curl -sSL https://get.haskellstack.org/ | sh

   # For macOS using Homebrew
   brew install haskell-stack

2. Required system libraries: 
    ```bash
    # For Ubuntu/Debian
    sudo apt-get update
    sudo apt-get install libgmp-dev

    # For macOS
    brew install gmp

## Installation

1. Clone the repository:
    ```bash
    git clone repourl
    cd OptionPricingSim

2. Build the project:
    ```bash
    stack build

## Running the Application

### Start the server:
    ```bash
    stack exec OptionPricingSim
    ```

The server will start on port 8080.

## API Usage
The service exposes a single endpoint for option pricing:

    ```bash
    curl -X POST http://localhost:8080/price \
     -H "Content-Type: application/json" \
     -d '{
         "initialStock": 100.0,
         "strikePrice": 110.0,
         "interestRate": 0.05,
         "volatility": 0.2,
         "timeToMaturity": 1.0,
         "numSimulations": 10000,
         "optionType": "Call"
     }'
    ```
## Parameters:
- **initialStock**: Current stock price
- **strikePrice**: Option strike price
- **interestRate**: Annual risk-free interest rate (e.g., 0.05 for 5%)
- **volatility**: Annual volatility (e.g., 0.2 for 20%)
- **timeToMaturity**: Time to expiration in years (e.g., 1.0 for one year)
- **numSimulations**: Number of simulations for Monte Carlo method
- **optionType**: Either "Call" or "Put"

Example Response:
{
    "price": 5.13254
}

## Testing

Run the test suite:

    ```bash
    stack test
    ```

## Test Cases
The project includes multiple test suites:

1. Black-Scholes Tests (test/BlackScholesSpec.hs)
    - Validates the Black-Scholes implementation against known option prices
    - Tests both Call and Put options
    - Checks pricing accuracy for different market conditions

2. Monte Carlo Tests (test/MonteCarloSpec.hs):
    - Verifies Monte Carlo simulation results against Black-Scholes prices
    - Tests convergence with increasing number of simulations
    - Checks both Call and Put options

3. Finite Difference Tests (test/FiniteDifferenceSpec.hs)
    - Validates Crank-Nicolson method against Black-Scholes prices
    - Tests grid resolution effects
    - Checks accuracy for different option parameters

4. Server Tests (test/ServerSpec.hs):
    - Tests API endpoint functionality
    - Validates request/response handling
    - Checks error conditions

## Project Structure

OptionPricingSim/

├── app/

│   └── Main.hs             # Application entry point

├── src/

│   ├── Api.hs             # API type definitions

│   ├── Server.hs          # Server implementation

│   ├── Types.hs           # Common type definitions

│   ├── RandomWalk.hs      # Random walk implementation

│   ├── MonteCarlo.hs      # Monte Carlo pricing method

│   └── PricingMethods/

│       ├── BlackScholes.hs    # Black-Scholes implementation

│       └── FiniteDifference.hs # Finite difference method

└── test/

    ├── Spec.hs

    ├── BlackScholesSpec.hs

    ├── MonteCarloSpec.hs

    ├── FiniteDifferenceSpec.hs

    └── ServerSpec.hs

## Error Handling
The service includes error handling for:
    - Invalid input parameters
    - Numerical computation errors
    - Server errors

Error responses include appropriate HTTP status codes and error messages.

## Performance Considerations
    - Monte Carlo simulation accuracy improves with higher numSimulations but requires more computation time
    - Finite Difference method accuracy depends on grid size
    - For quick price estimates, the Black-Scholes method is recommended
    - For more complex scenarios, Monte Carlo simulation provides flexibility


