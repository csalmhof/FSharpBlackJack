# BlackJack

A small BlackJackGame. It should showcase how to write a small Single Page Application using Fable, Elmish and the Fulma component library.

## How to run the app?

First make sure that you have a recent version of the [.NET core SDK](https://dotnet.microsoft.com/download) as well as a current (minimum LTS) version of [Node.js](https://nodejs.org/en/) installed on your machine. I recommend to use [yarn](https://yarnpkg.com/en/docs/getting-started) as your node package manager and task runner for this app - npm should still work though.

In the root directory of this application execute the following commands to install the projects .NET and node dependencies.

```bash
dotnet restore
yarn install
```

To run the project execute the following command.

```bash
yarn start
```

You can reach your application by navigating to [http://localhost:8080](http://localhost:8080) after starting your project.
