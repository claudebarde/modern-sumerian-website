@module external styles: {..} = "./App.module.scss"

@react.component
let make = () => {
  <main>
    <Header />
    <Body />
    <Footer />
  </main>
}
