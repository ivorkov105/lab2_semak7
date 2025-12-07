package gui

import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.scene.Scene
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.collections.ObservableBuffer
import scalafx.Includes._
import HeadersList.HeadersList
import CustomTypes.{CustomDate, Point2D}
import java.io.File
import scala.util.{Try, Success, Failure}

object MainApp extends JFXApp3 {
  
  override def start(): Unit = {
    stage = new PrimaryStage {
      title = "HeadersList Manager - Lab2"
      scene = new Scene(900, 650) {
        val controller = new MainViewController()
        root = controller.createUI()
      }
    }
  }
}

class MainViewController {
  
  // Type selection - три отдельных списка для разных типов
  private var currentType: String = "CustomDate"
  private var headersListDate = new HeadersList[CustomDate]()
  private var headersListPoint = new HeadersList[Point2D]()
  private var headersListString = new HeadersList[String]()
  
  // Observable data for TableView
  private val tableData = ObservableBuffer[HeadersListRow]()
  
  private case class HeadersListRow(index: Int, header: String, listContent: String) {
    val indexProperty = scalafx.beans.property.ObjectProperty(index)
    val headerProperty = scalafx.beans.property.StringProperty(header)
    val listContentProperty = scalafx.beans.property.StringProperty(listContent)
  }
  
  def createUI(): BorderPane = {
    // ToggleButtons для выбора типа
    val tg = new ToggleGroup()
    val btnCustomDate = new ToggleButton("CustomDate") {
      toggleGroup = tg
      selected = true
    }
    val btnPoint2D = new ToggleButton("Point2D") {
      toggleGroup = tg
    }
    val btnString = new ToggleButton("String") {
      toggleGroup = tg
    }
    
    tg.selectedToggle.onChange { (_, _, newValue) =>
      if (newValue != null) {
        currentType = newValue.asInstanceOf[javafx.scene.control.ToggleButton].getText
        refreshTable()
      }
    }
    
    new BorderPane {
      padding = Insets(10)
      
      // Top: Title and type selector
      top = new VBox(5) {
        children ++= Seq[javafx.scene.Node](
          new HBox(10) {
            children ++= Seq[javafx.scene.Node](
              new Label("HeadersList Manager") {
                style = "-fx-font-size: 20px; -fx-font-weight: bold;"
              },
              new Region { hgrow = javafx.scene.layout.Priority.ALWAYS },
              new Label("Type:") {
                style = "-fx-font-size: 14px;"
              },
              btnCustomDate,
              btnPoint2D,
              btnString
            )
          },
          new Separator()
        )
        padding = Insets(0, 0, 10, 0)
      }
      
      // Center: TableView
      center = createTableView()
      
      // Right: Control buttons
      right = createControlPanel()
      
      // Bottom: Status bar
      bottom = new Label("Ready. Select type and add elements.") {
        id = "statusLabel"
        padding = Insets(10, 0, 0, 0)
      }
    }
  }
  
  private def createTableView(): TableView[HeadersListRow] = {
    new TableView[HeadersListRow](tableData) {
      columns ++= List(
        new TableColumn[HeadersListRow, Int] {
          text = "Index"
          cellValueFactory = _.value.indexProperty
          prefWidth = 60
        },
        new TableColumn[HeadersListRow, String] {
          text = "Header"
          cellValueFactory = _.value.headerProperty
          prefWidth = 250
        },
        new TableColumn[HeadersListRow, String] {
          text = "List Content"
          cellValueFactory = _.value.listContentProperty
          prefWidth = 550
        }
      )
      columnResizePolicy = TableView.ConstrainedResizePolicy
    }
  }
  
  private def createControlPanel(): VBox = {
    new VBox(10) {
      padding = Insets(0, 0, 0, 10)
      children ++= Seq[javafx.scene.Node](
        new Label("Add Operations:") {
          style = "-fx-font-weight: bold;"
        },
        new Button("Add Element") {
          maxWidth = Double.MaxValue
          onAction = _ => handleAdd()
        },
        new Button("Add by Index") {
          maxWidth = Double.MaxValue
          onAction = _ => handleAddByIndex()
        },
        new Button("Add Sorted") {
          maxWidth = Double.MaxValue
          onAction = _ => handleAddSorted()
        },
        new Separator(),
        new Label("Modify Operations:") {
          style = "-fx-font-weight: bold;"
        },
        new Button("Remove by Index") {
          maxWidth = Double.MaxValue
          onAction = _ => handleRemove()
        },
        new Button("Sort (Imperative)") {
          maxWidth = Double.MaxValue
          onAction = _ => handleSort(imperative = true)
        },
        new Button("Sort (Functional)") {
          maxWidth = Double.MaxValue
          style = "-fx-background-color: #4CAF50; -fx-text-fill: white;"
          onAction = _ => handleSort(imperative = false)
        },
        new Button("Balance Lists") {
          maxWidth = Double.MaxValue
          onAction = _ => handleBalance()
        },
        new Separator(),
        new Label("Query Operations:") {
          style = "-fx-font-weight: bold;"
        },
        new Button("Find by Header") {
          maxWidth = Double.MaxValue
          onAction = _ => handleFindByHeader()
        },
        new Button("Print All Headers") {
          maxWidth = Double.MaxValue
          onAction = _ => handlePrintHeaders()
        },
        new Separator(),
        new Label("File Operations:") {
          style = "-fx-font-weight: bold;"
        },
        new Button("Save to JSON") {
          maxWidth = Double.MaxValue
          onAction = _ => handleSave()
        },
        new Button("Load from JSON") {
          maxWidth = Double.MaxValue
          onAction = _ => handleLoad()
        },
        new Button("Save to Binary") {
          maxWidth = Double.MaxValue
          style = "-fx-background-color: #FF9800; -fx-text-fill: white;"
          onAction = _ => handleSaveBinary()
        },
        new Button("Load from Binary") {
          maxWidth = Double.MaxValue
          style = "-fx-background-color: #FF9800; -fx-text-fill: white;"
          onAction = _ => handleLoadBinary()
        },
        new Separator(),
        new Label("Utility:") {
          style = "-fx-font-weight: bold;"
        },
        new Button("Add Sample Data") {
          maxWidth = Double.MaxValue
          style = "-fx-background-color: #2196F3; -fx-text-fill: white;"
          onAction = _ => handleAddSampleData()
        },
        new Button("Clear All") {
          maxWidth = Double.MaxValue
          style = "-fx-background-color: #f44336; -fx-text-fill: white;"
          onAction = _ => handleClear()
        }
      )
      prefWidth = 220
    }
  }
  
  private def getCurrentList(): Any = {
    currentType match {
      case "CustomDate" => headersListDate
      case "Point2D" => headersListPoint
      case "String" => headersListString
      case _ => headersListDate
    }
  }
  
  private def getCurrentSize(): Int = {
    currentType match {
      case "CustomDate" => headersListDate.size
      case "Point2D" => headersListPoint.size
      case "String" => headersListString.size
      case _ => 0
    }
  }
  
  private def handleAdd(): Unit = {
    val hint = currentType match {
      case "Point2D" => "Header (x,y): 10.5,20.3"
      case "CustomDate" => "Header (dd/MM/yyyy): 25/12/2024"
      case "String" => "Header: My Header"
      case _ => ""
    }
    
    val dialog = new TextInputDialog() {
      title.value = "Add New Element"
      headerText.value = s"Add a new header for $currentType"
      contentText.value = hint
    }
    
    val result = dialog.showAndWait()
    result match {
      case Some(headerStr) =>
        currentType match {
          case "CustomDate" =>
            Try(CustomDate.parse(headerStr)) match {
              case Success(header) => askForListItems(header)
              case Failure(e) => showError("Invalid date format", e.getMessage)
            }
          case "Point2D" =>
            Try(Point2D.parse(headerStr)) match {
              case Success(header) => askForListItems(header)
              case Failure(e) => showError("Invalid point format", e.getMessage)
            }
          case "String" =>
            askForListItems(headerStr)
        }
      case None => // Cancelled
    }
  }
  
  private def askForListItems[T <: Comparable[T]](header: T): Unit = {
    val hint = currentType match {
      case "Point2D" => "Items (x,y comma-separated): 1.0,2.0, 3.0,4.0"
      case "CustomDate" => "Items (dd/MM/yyyy, comma-separated): 01/01/2024, 02/01/2024"
      case "String" => "Items (comma-separated): item1, item2, item3"
      case _ => ""
    }
    
    val listDialog = new TextInputDialog() {
      title.value = "Add List Items"
      headerText.value = "Add list items for this header"
      contentText.value = hint
    }
    
    listDialog.showAndWait() match {
      case Some(listStr) =>
        val items = currentType match {
          case "CustomDate" =>
            listStr.split(",").map(_.trim).flatMap { item =>
              Try(CustomDate.parse(item)).toOption
            }.toList.asInstanceOf[List[T]]
          case "Point2D" =>
            listStr.split(",").map(_.trim).flatMap { item =>
              Try(Point2D.parse(item)).toOption
            }.toList.asInstanceOf[List[T]]
          case "String" =>
            listStr.split(",").map(_.trim).toList.asInstanceOf[List[T]]
          case _ => List.empty[T]
        }
        
        currentType match {
          case "CustomDate" =>
            headersListDate.add(header.asInstanceOf[CustomDate], items.asInstanceOf[List[CustomDate]])
          case "Point2D" =>
            headersListPoint.add(header.asInstanceOf[Point2D], items.asInstanceOf[List[Point2D]])
          case "String" =>
            headersListString.add(header.asInstanceOf[String], items.asInstanceOf[List[String]])
        }
        
        refreshTable()
        updateStatus(s"Added header with ${items.length} items")
        
      case None => // Cancelled
    }
  }
  
  private def handleRemove(): Unit = {
    if (getCurrentSize() > 0) {
      val dialog = new TextInputDialog("0") {
        title.value = "Remove Element"
        headerText.value = "Remove element by index"
        contentText.value = s"Index (0-${getCurrentSize() - 1}):"
      }
      
      dialog.showAndWait() match {
        case Some(indexStr) =>
          Try(indexStr.toInt) match {
            case Success(index) if index >= 0 && index < getCurrentSize() =>
              currentType match {
                case "CustomDate" => headersListDate.remove(index)
                case "Point2D" => headersListPoint.remove(index)
                case "String" => headersListString.remove(index)
              }
              refreshTable()
              updateStatus(s"Removed element at index $index")
            case _ =>
              showError("Invalid Index", "Please enter a valid index")
          }
        case None => // Cancelled
      }
    } else {
      showError("Empty List", "The list is empty")
    }
  }
  
  private def handleAddByIndex(): Unit = {
    val dialog = new TextInputDialog("0") {
      title.value = "Add by Index"
      headerText.value = s"Add element at specific index for $currentType"
      contentText.value = "Index:"
    }
    
    dialog.showAndWait() match {
      case Some(indexStr) =>
        Try(indexStr.toInt) match {
          case Success(index) if index >= 0 && index <= getCurrentSize() =>
            askForHeaderAndList((header, items) => {
              currentType match {
                case "CustomDate" =>
                  headersListDate.add(index, header.asInstanceOf[CustomDate], items.asInstanceOf[List[CustomDate]])
                case "Point2D" =>
                  headersListPoint.add(index, header.asInstanceOf[Point2D], items.asInstanceOf[List[Point2D]])
                case "String" =>
                  headersListString.add(index, header.asInstanceOf[String], items.asInstanceOf[List[String]])
              }
              refreshTable()
              updateStatus(s"Added element at index $index")
            })
          case _ =>
            showError("Invalid Index", s"Please enter index between 0 and ${getCurrentSize()}")
        }
      case None => // Cancelled
    }
  }
  
  private def handleAddSorted(): Unit = {
    askForHeaderAndList((header, items) => {
      currentType match {
        case "CustomDate" =>
          headersListDate.addSorted(header.asInstanceOf[CustomDate], items.asInstanceOf[List[CustomDate]])
        case "Point2D" =>
          headersListPoint.addSorted(header.asInstanceOf[Point2D], items.asInstanceOf[List[Point2D]])
        case "String" =>
          headersListString.addSorted(header.asInstanceOf[String], items.asInstanceOf[List[String]])
      }
      refreshTable()
      updateStatus("Added element in sorted position")
    })
  }
  
  private def askForHeaderAndList(callback: (Any, List[Any]) => Unit): Unit = {
    val hint = currentType match {
      case "Point2D" => "Header (x,y): 10.5,20.3"
      case "CustomDate" => "Header (dd/MM/yyyy): 25/12/2024"
      case "String" => "Header: My Header"
      case _ => ""
    }
    
    val dialog = new TextInputDialog() {
      title.value = "Add Element"
      headerText.value = s"Enter header for $currentType"
      contentText.value = hint
    }
    
    dialog.showAndWait() match {
      case Some(headerStr) =>
        currentType match {
          case "CustomDate" =>
            Try(CustomDate.parse(headerStr)) match {
              case Success(header) => askForListItemsGeneric(header, callback)
              case Failure(e) => showError("Invalid date format", e.getMessage)
            }
          case "Point2D" =>
            Try(Point2D.parse(headerStr)) match {
              case Success(header) => askForListItemsGeneric(header, callback)
              case Failure(e) => showError("Invalid point format", e.getMessage)
            }
          case "String" =>
            askForListItemsGeneric(headerStr, callback)
        }
      case None => // Cancelled
    }
  }
  
  private def askForListItemsGeneric(header: Any, callback: (Any, List[Any]) => Unit): Unit = {
    val hint = currentType match {
      case "Point2D" => "Items (x,y comma-separated): 1.0,2.0, 3.0,4.0"
      case "CustomDate" => "Items (dd/MM/yyyy, comma-separated): 01/01/2024, 02/01/2024"
      case "String" => "Items (comma-separated): item1, item2, item3"
      case _ => ""
    }
    
    val listDialog = new TextInputDialog() {
      title.value = "Add List Items"
      headerText.value = "Add list items for this header"
      contentText.value = hint
    }
    
    listDialog.showAndWait() match {
      case Some(listStr) =>
        val items = currentType match {
          case "CustomDate" =>
            listStr.split(",").map(_.trim).flatMap { item =>
              Try(CustomDate.parse(item)).toOption
            }.toList
          case "Point2D" =>
            listStr.split(",").map(_.trim).flatMap { item =>
              Try(Point2D.parse(item)).toOption
            }.toList
          case "String" =>
            listStr.split(",").map(_.trim).toList
          case _ => List.empty
        }
        callback(header, items.asInstanceOf[List[Any]])
      case None => // Cancelled
    }
  }
  
  private def handleFindByHeader(): Unit = {
    val hint = currentType match {
      case "Point2D" => "Header (x,y): 10.5,20.3"
      case "CustomDate" => "Header (dd/MM/yyyy): 25/12/2024"
      case "String" => "Header: My Header"
      case _ => ""
    }
    
    val dialog = new TextInputDialog() {
      title.value = "Find by Header"
      headerText.value = s"Find list by header for $currentType"
      contentText.value = hint
    }
    
    dialog.showAndWait() match {
      case Some(headerStr) =>
        val result = currentType match {
          case "CustomDate" =>
            Try(CustomDate.parse(headerStr)) match {
              case Success(header) => headersListDate.getListByHeader(header)
              case Failure(e) => 
                showError("Invalid format", e.getMessage)
                None
            }
          case "Point2D" =>
            Try(Point2D.parse(headerStr)) match {
              case Success(header) => headersListPoint.getListByHeader(header)
              case Failure(e) =>
                showError("Invalid format", e.getMessage)
                None
            }
          case "String" =>
            headersListString.getListByHeader(headerStr)
          case _ => None
        }
        
        result match {
          case Some(list) =>
            new Alert(Alert.AlertType.Information) {
              title = "Found List"
              headerText = s"List for header: $headerStr"
              contentText = list.map(_.toString).mkString(", ")
            }.showAndWait()
          case None =>
            new Alert(Alert.AlertType.Information) {
              title = "Not Found"
              headerText = "No list found"
              contentText = s"No list found for header: $headerStr"
            }.showAndWait()
        }
      case None => // Cancelled
    }
  }
  
  private def handlePrintHeaders(): Unit = {
    val headers = scala.collection.mutable.ListBuffer[String]()
    
    currentType match {
      case "CustomDate" =>
        headersListDate.forEachHeader(header => headers += header.toString)
      case "Point2D" =>
        headersListPoint.forEachHeader(header => headers += header.toString)
      case "String" =>
        headersListString.forEachHeader(header => headers += header)
    }
    
    if (headers.nonEmpty) {
      new Alert(Alert.AlertType.Information) {
        title = "All Headers"
        headerText = s"Total: ${headers.length} headers"
        contentText = headers.mkString("\n")
      }.showAndWait()
      updateStatus(s"Printed ${headers.length} headers")
    } else {
      showError("Empty List", "No headers to print")
    }
  }
  
  private def handleSort(imperative: Boolean): Unit = {
    currentType match {
      case "CustomDate" =>
        if (imperative) headersListDate.sort() else headersListDate.sortFunctional()
      case "Point2D" =>
        if (imperative) headersListPoint.sort() else headersListPoint.sortFunctional()
      case "String" =>
        if (imperative) headersListString.sort() else headersListString.sortFunctional()
    }
    
    val method = if (imperative) "imperative QuickSort" else "functional MergeSort"
    updateStatus(s"Sorted using $method")
    refreshTable()
  }
  
  private def handleBalance(): Unit = {
    currentType match {
      case "CustomDate" => headersListDate.balance()
      case "Point2D" => headersListPoint.balance()
      case "String" => headersListString.balance()
    }
    refreshTable()
    updateStatus("List balanced")
  }
  
  private def handleSave(): Unit = {
    val fileName = s"${currentType}_list.json"
    val file = new File(fileName)
    
    Try {
      currentType match {
        case "CustomDate" => headersListDate.saveToFile(file)
        case "Point2D" => headersListPoint.saveToFile(file)
        case "String" => headersListString.saveToFile(file)
      }
    } match {
      case Success(_) =>
        updateStatus(s"Saved to $fileName")
        new Alert(Alert.AlertType.Information) {
          title = "Save Success"
          headerText = ""
          contentText = s"Data saved to $fileName"
        }.showAndWait()
      case Failure(e) =>
        showError("Save Error", e.getMessage)
    }
  }
  
  private def handleLoad(): Unit = {
    val fileName = s"${currentType}_list.json"
    val file = new File(fileName)
    
    if (!file.exists()) {
      showError("Load Error", s"File $fileName not found")
      return
    }
    
    Try {
      currentType match {
        case "CustomDate" =>
          val loaded = HeadersList.loadFromFile(file, classOf[CustomDate])
          headersListDate = loaded
        case "Point2D" =>
          val loaded = HeadersList.loadFromFile(file, classOf[Point2D])
          headersListPoint = loaded
        case "String" =>
          val loaded = HeadersList.loadFromFile(file, classOf[String])
          headersListString = loaded
      }
    } match {
      case Success(_) =>
        refreshTable()
        updateStatus(s"Loaded from $fileName")
        new Alert(Alert.AlertType.Information) {
          title = "Load Success"
          headerText = ""
          contentText = s"Data loaded from $fileName"
        }.showAndWait()
      case Failure(e) =>
        showError("Load Error", e.getMessage)
    }
  }
  
  private def handleSaveBinary(): Unit = {
    val fileName = s"${currentType}_list.bin"
    val file = new File(fileName)
    
    Try {
      currentType match {
        case "CustomDate" => headersListDate.saveToBinaryFile(file)
        case "Point2D" => headersListPoint.saveToBinaryFile(file)
        case "String" => headersListString.saveToBinaryFile(file)
      }
    } match {
      case Success(_) =>
        updateStatus(s"Saved to binary $fileName")
        new Alert(Alert.AlertType.Information) {
          title = "Binary Save Success"
          headerText = ""
          contentText = s"Data saved to binary file $fileName"
        }.showAndWait()
      case Failure(e) =>
        showError("Binary Save Error", e.getMessage)
    }
  }
  
  private def handleLoadBinary(): Unit = {
    val fileName = s"${currentType}_list.bin"
    val file = new File(fileName)
    
    if (!file.exists()) {
      showError("Load Error", s"Binary file $fileName not found")
      return
    }
    
    Try {
      currentType match {
        case "CustomDate" =>
          val loaded = HeadersList.loadFromBinaryFile(file, classOf[CustomDate])
          headersListDate = loaded
        case "Point2D" =>
          val loaded = HeadersList.loadFromBinaryFile(file, classOf[Point2D])
          headersListPoint = loaded
        case "String" =>
          val loaded = HeadersList.loadFromBinaryFile(file, classOf[String])
          headersListString = loaded
      }
    } match {
      case Success(_) =>
        refreshTable()
        updateStatus(s"Loaded from binary $fileName")
        new Alert(Alert.AlertType.Information) {
          title = "Binary Load Success"
          headerText = ""
          contentText = s"Data loaded from binary file $fileName"
        }.showAndWait()
      case Failure(e) =>
        showError("Binary Load Error", e.getMessage)
    }
  }
  
  private def handleAddSampleData(): Unit = {
    currentType match {
      case "CustomDate" =>
        val sampleData = List(
          (CustomDate.of(15, 3, 2024), List(
            CustomDate.of(1, 1, 2024),
            CustomDate.of(2, 1, 2024),
            CustomDate.of(3, 1, 2024)
          )),
          (CustomDate.of(10, 5, 2024), List(
            CustomDate.of(4, 2, 2024),
            CustomDate.of(5, 2, 2024)
          )),
          (CustomDate.of(20, 1, 2024), List(
            CustomDate.of(6, 3, 2024),
            CustomDate.of(7, 3, 2024),
            CustomDate.of(8, 3, 2024),
            CustomDate.of(9, 3, 2024)
          ))
        )
        sampleData.foreach { case (header, list) =>
          headersListDate.add(header, list)
        }
        
      case "Point2D" =>
        val sampleData = List(
          (Point2D.of(10.0, 20.0), List(
            Point2D.of(1.0, 1.0),
            Point2D.of(2.0, 2.0),
            Point2D.of(3.0, 3.0)
          )),
          (Point2D.of(5.5, 10.5), List(
            Point2D.of(4.0, 4.0),
            Point2D.of(5.0, 5.0)
          )),
          (Point2D.of(15.0, 25.0), List(
            Point2D.of(6.0, 6.0),
            Point2D.of(7.0, 7.0),
            Point2D.of(8.0, 8.0),
            Point2D.of(9.0, 9.0)
          ))
        )
        sampleData.foreach { case (header, list) =>
          headersListPoint.add(header, list)
        }
        
      case "String" =>
        val sampleData = List(
          ("Header 1", List("Item A", "Item B", "Item C")),
          ("Header 2", List("Item D", "Item E")),
          ("Header 3", List("Item F", "Item G", "Item H", "Item I"))
        )
        sampleData.foreach { case (header, list) =>
          headersListString.add(header, list)
        }
    }
    
    refreshTable()
    updateStatus(s"Sample $currentType data added")
  }
  
  private def handleClear(): Unit = {
    currentType match {
      case "CustomDate" =>
        while (headersListDate.size > 0) {
          headersListDate.remove(0)
        }
      case "Point2D" =>
        while (headersListPoint.size > 0) {
          headersListPoint.remove(0)
        }
      case "String" =>
        while (headersListString.size > 0) {
          headersListString.remove(0)
        }
    }
    refreshTable()
    updateStatus("List cleared")
  }
  
  private def refreshTable(): Unit = {
    tableData.clear()
    currentType match {
      case "CustomDate" =>
        for (i <- 0 until headersListDate.size) {
          val header = headersListDate.getHeader(i)
          val list = headersListDate.getList(i)
          tableData += HeadersListRow(
            i,
            header.toString,
            list.map(_.toString).mkString(", ")
          )
        }
      case "Point2D" =>
        for (i <- 0 until headersListPoint.size) {
          val header = headersListPoint.getHeader(i)
          val list = headersListPoint.getList(i)
          tableData += HeadersListRow(
            i,
            header.toString,
            list.map(_.toString).mkString(", ")
          )
        }
      case "String" =>
        for (i <- 0 until headersListString.size) {
          val header = headersListString.getHeader(i)
          val list = headersListString.getList(i)
          tableData += HeadersListRow(
            i,
            header,
            list.mkString(", ")
          )
        }
    }
  }
  
  private def updateStatus(message: String): Unit = {
    println(s"Status: $message")
  }
  
  private def showError(titleText: String, message: String): Unit = {
    new Alert(Alert.AlertType.Error) {
      title = titleText
      headerText = ""
      contentText = message
    }.showAndWait()
  }
}
