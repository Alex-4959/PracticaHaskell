# Energy-Stabilization-of-the-Monad-Orbital-Station

Skeleton PdP 2526 Haskell

## Instructions for Running the Code

1. Ensure you have `ghci` installed on your system. You can download it from [here](https://www.haskell.org/ghcup/install/).

2. Clone this repository to your local machine using the following command:

   ```
   git clone https://github.com/wilberquito/Energy-Stabilization-of-the-Monad-Orbital-Station.git
   ```

3. Navigate to the project directory:

   ```
   cd Energy-Stabilization-of-the-Monad-Orbital-Station
   ```

4. Load the files into `ghci` or compile the code using the following command:

    ```
    ghci *.hs
    ```

5. For testing, import the module in `ghci`:

    ```
    ghci> import Main
    ```

    or multiple modules at once:

    ```
    ghci> :m + Main Parser Controller
    ```

6. The game must start when running the function `main`:

    ```
    ghci> :m + Main
    ghci> main
    Instance path: instancies/facils.txt
    . . .
    ```