@module external styles: {..} = "./App.module.scss"

@react.component
let make = () => {
  <main>
    <Header />
    <Body />
    <div>
      {"footer"->React.string}
    </div>
  </main>
}
