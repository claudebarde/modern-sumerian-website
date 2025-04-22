@module external styles: {..} = "./App.module.scss"

@react.component
let make = () => {
  [<Header key="header" />, <Body key="body" />, <Footer key="footer" />]->React.array
}
