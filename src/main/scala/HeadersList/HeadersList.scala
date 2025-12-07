package HeadersList

import com.google.gson.{Gson, GsonBuilder}
import com.google.gson.reflect.TypeToken
import java.io.{File, FileReader, FileWriter, DataOutputStream, DataInputStream, FileOutputStream, FileInputStream}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import HeadersList._

//ъуъ
class HeadersList[T <: Comparable[T]] extends Iterable[HeadersList.SerializableNode[T]] {

  private var root: Option[HeaderNode[T]] = None
  private var _size: Int = 0
  
  override def iterator: Iterator[SerializableNode[T]] = new Iterator[SerializableNode[T]] {
    private var current = root
    
    override def hasNext: Boolean = current.isDefined
    
    override def next(): SerializableNode[T] = {
      if (!hasNext) throw new NoSuchElementException
      val node = current.get
      val data = SerializableNode(node.header, node.associatedList.toList)
      current = node.next
      data
    }
  }
  
  //в конец добавляем списачек и загаловочек
  def add(header: T, list: List[T]): Unit = {
    val newNode = HeaderNode(header, ListBuffer(list: _*), None)
    root match {
      case None => root = Some(newNode)
      case Some(r) =>
        var currNode = r
        while (currNode.next.isDefined) {
          currNode = currNode.next.get
        }
        currNode.next = Some(newNode)
    }
    _size += 1
  }
  
  //тотальнейшая вставка списка, его башки ослиной по индексу
  def add(index: Int, header: T, list: List[T]): Unit = {
    require(index >= 0 && index <= _size, "ты чё твориш...")
    val newNode = HeaderNode(header, ListBuffer(list: _*), None)
    if (index == 0) {
      newNode.next = root
      root = Some(newNode)
    } else {
      var currNode = root.get
      for (_ <- 0 until index - 1) {
        currNode = currNode.next.get
      }
      newNode.next = currNode.next
      currNode.next = Some(newNode)
    }
    _size += 1
  }
  
  //сортиров очка + балансиров очка (императивная с мутациями)
  def sort(): Unit = {
    if (_size >= 2) {
      var last = root.get
      while (last.next.isDefined) {
        last = last.next.get
      }
      quickSort(root.get, last)
    }
  }
  
  private def quickSort(start: HeaderNode[T], end: HeaderNode[T]): Unit = {
    if (start == end || start.next.contains(end)) return
    
    val pivot = partition(start, end)
    
    if (pivot != start) {
      var prevPivot = start
      while (!prevPivot.next.contains(pivot)) {
        prevPivot = prevPivot.next.get
      }
      quickSort(start, prevPivot)
    }
    
    if (pivot != end && pivot.next.isDefined) {
      quickSort(pivot.next.get, end)
    }
  }
  
  private def partition(start: HeaderNode[T], end: HeaderNode[T]): HeaderNode[T] = {
    val pivotValue = end.header
    var i = start
    var j = start
    
    while (j != end) {
      if (j.header.compareTo(pivotValue) < 0) {
        swapNodes(i, j)
        i = i.next.get
      }
      j = j.next.get
    }
    
    swapNodes(i, end)
    i
  }
  
  private def swapNodes(node1: HeaderNode[T], node2: HeaderNode[T]): Unit = {
    if (node1 != node2) {
      val tempHeader = node1.header
      val tempList = node1.associatedList
      
      node1.header = node2.header
      node1.associatedList = node2.associatedList
      
      node2.header = tempHeader
      node2.associatedList = tempList
    }
  }
  
  //функциональная сортировка (merge sort без мутаций)
  def sortFunctional(): Unit = {
    root = functionalMergeSort(root)
  }
  
  private def functionalMergeSort(head: Option[HeaderNode[T]]): Option[HeaderNode[T]] = {
    head match {
      case None | Some(HeaderNode(_, _, None)) => head
      case Some =>
        val (left, right) = split(head)
        merge(functionalMergeSort(left), functionalMergeSort(right))
    }
  }
  
  private def split(head: Option[HeaderNode[T]]): (Option[HeaderNode[T]], Option[HeaderNode[T]]) = {
    @scala.annotation.tailrec
    def findMiddle(slow: HeaderNode[T], fast: Option[HeaderNode[T]]): HeaderNode[T] = {
      fast.flatMap(_.next).flatMap(_.next) match {
        case Some(nextFast) => findMiddle(slow.next.get, Some(nextFast))
        case None => slow
      }
    }
    
    head match {
      case None => (None, None)
      case Some(h) =>
        val middle = findMiddle(h, head)
        val secondHalf = middle.next
        middle.next = None
        (head, secondHalf)
    }
  }
  
  private def merge(left: Option[HeaderNode[T]], right: Option[HeaderNode[T]]): Option[HeaderNode[T]] = {
    (left, right) match {
      case (None, r) => r
      case (l, None) => l
      case (Some(l), Some(r)) =>
        if (l.header.compareTo(r.header) <= 0) {
          Some(HeaderNode(l.header, l.associatedList.clone(), merge(l.next, right)))
        } else {
          Some(HeaderNode(r.header, r.associatedList.clone(), merge(left, r.next)))
        }
    }
  }
  
  def balance(): Unit = {
    if (root.isEmpty || root.get.next.isEmpty) return
    
    val allItems = ListBuffer[T]()
    var current = root
    while (current.isDefined) {
      allItems ++= current.get.associatedList
      current = current.get.next
    }
    
    current = root
    while (current.isDefined) {
      current.get.associatedList.clear()
      current = current.get.next
    }
    
    val totalItems = allItems.size
    val numLists = _size
    val baseSize = totalItems / numLists
    val remainder = totalItems % numLists
    
    var currentItemIndex = 0
    var nodeIndex = 0
    current = root
    
    while (current.isDefined) {
      val sublistSize = baseSize + (if (nodeIndex < remainder) 1 else 0)
      
      for (_ <- 0 until sublistSize if currentItemIndex < totalItems) {
        current.get.associatedList += allItems(currentItemIndex)
        currentItemIndex += 1
      }
      
      nodeIndex += 1
      current = current.get.next
    }
  }
  
  //сортир... Нажмите для продолжения...
  def addSorted(header: T, list: List[T]): Unit = {
    val newNode = HeaderNode(header, ListBuffer(list: _*), None)
    
    root match {
      case None =>
        root = Some(newNode)
        _size += 1
        return
      case Some(r) =>
        val comparison = header.compareTo(r.header)
        if (comparison < 0) {
          newNode.next = root
          root = Some(newNode)
          _size += 1
          return
        }
        if (comparison == 0) {
          r.associatedList ++= list
          return
        }
    }
    
    var current = root.get
    while (current.next.isDefined) {
      val comparison = header.compareTo(current.next.get.header)
      
      if (comparison == 0) {
        current.next.get.associatedList ++= list
        return
      }
      
      if (comparison < 0) {
        newNode.next = current.next
        current.next = Some(newNode)
        _size += 1
        return
      }
      current = current.next.get
    }
    current.next = Some(newNode)
    _size += 1
  }
  
  //бошку открути
  def getHeader(index: Int): T = {
    require(index >= 0 && index < _size, "ты чё делаешь? мужик, успокойся")
    var curr = root.get
    for (_ <- 0 until index) {
      curr = curr.next.get
    }
    curr.header
  }
  
  //вы списков продаете?
  //нет тока показываю
  //красивое...
  def getList(index: Int): List[T] = {
    require(index >= 0 && index < _size, "ты чё делаешь? мужик, успокойся")
    var curr = root.get
    for (_ <- 0 until index) {
      curr = curr.next.get
    }
    curr.associatedList.toList
  }
  
  //списачик палучаим дада
  def getListByHeader(header: T): Option[List[T]] = {
    var curr = root
    while (curr.isDefined) {
      if (curr.get.header == header) {
        return Some(curr.get.associatedList.toList)
      }
      curr = curr.get.next
    }
    None
  }
  
  //минус элемент
  def remove(index: Int): Unit = {
    require(index >= 0 && index < _size, "да харош уже нет такого индекса")
    
    if (index == 0) {
      root = root.get.next
      _size -= 1
      return
    }
    
    var prev = root.get
    for (_ <- 0 until index - 1) {
      prev = prev.next.get
    }
    val removingNode = prev.next.get
    prev.next = removingNode.next
    _size -= 1
  }
  
  //да это на морозе уменьшился чесно
  override def size: Int = _size
  
  //вешаем итератор
  def forEachHeader(action: T => Unit): Unit = {
    var curr = root
    while (curr.isDefined) {
      action(curr.get.header)
      curr = curr.get.next
    }
  }
  
  //парсуем, я сказала парсуем!
  def saveToFile(file: File): Unit = {
    import CustomTypes.{CustomDateAdapter, Point2DAdapter}
    
    val dataToSave = new java.util.ArrayList[java.util.Map[String, Any]]()
    
    var current = root
    while (current.isDefined) {
      val node = new java.util.HashMap[String, Any]()
      node.put("header", current.get.header)
      node.put("associatedList", current.get.associatedList.toList.asJava)
      dataToSave.add(node)
      current = current.get.next
    }
    
    val gson = new GsonBuilder()
      .setPrettyPrinting()
      .registerTypeAdapter(classOf[CustomTypes.CustomDate], CustomDateAdapter)
      .registerTypeAdapter(classOf[CustomTypes.Point2D], Point2DAdapter)
      .create()
    
    val writer = new FileWriter(file)
    try {
      gson.toJson(dataToSave, writer)
    } finally {
      writer.close()
    }
  }
  
  //бинарная сериализация
  def saveToBinaryFile(file: File): Unit = {
    val dos = new DataOutputStream(new FileOutputStream(file))
    try {
      dos.writeInt(_size)
      
      var current = root
      while (current.isDefined) {
        val node = current.get
        writeComparable(dos, node.header)
        dos.writeInt(node.associatedList.size)
        node.associatedList.foreach { item =>
          writeComparable(dos, item)
        }
        current = node.next
      }
    } finally {
      dos.close()
    }
  }
  
  private def writeComparable(dos: DataOutputStream, value: T): Unit = {
    value match {
      case date: CustomTypes.CustomDate =>
        dos.writeUTF("CustomDate")
        dos.writeInt(date.getDay)
        dos.writeInt(date.getMonth)
        dos.writeInt(date.getYear)
      case point: CustomTypes.Point2D =>
        dos.writeUTF("Point2D")
        dos.writeDouble(point.getX)
        dos.writeDouble(point.getY)
      case str: String =>
        dos.writeUTF("String")
        dos.writeUTF(str)
      case _ =>
        dos.writeUTF("Unknown")
        dos.writeUTF(value.toString)
    }
  }
  
  private def readComparable(dis: DataInputStream, clazz: Class[T]): T = {
    val typeName = dis.readUTF()
    
    typeName match {
      case "CustomDate" =>
        val day = dis.readInt()
        val month = dis.readInt()
        val year = dis.readInt()
        CustomTypes.CustomDate.of(day, month, year).asInstanceOf[T]
      case "Point2D" =>
        val x = dis.readDouble()
        val y = dis.readDouble()
        CustomTypes.Point2D.of(x, y).asInstanceOf[T]
      case "String" =>
        dis.readUTF().asInstanceOf[T]
      case "Unknown" =>
        dis.readUTF().asInstanceOf[T]
    }
  }
  
  override def toString: String = {
    if (root.isEmpty) return "HeadersList{[]}"
    
    val sb = new StringBuilder("HeadersList{[")
    var current = root
    while (current.isDefined) {
      sb.append(current.get.header.toString)
        .append(": ")
        .append(current.get.associatedList.mkString("[", ", ", "]"))
      if (current.get.next.isDefined) {
        sb.append("], [")
      }
      current = current.get.next
    }
    sb.append("]}").toString
  }
  
  override def equals(obj: Any): Boolean = obj match {
    case that: HeadersList[_] =>
      if (this._size != that._size) return false
      
      var currentThis = this.root
      var currentThat = that.root
      
      while (currentThis.isDefined) {
        if (currentThis.get.header != currentThat.get.header ||
            currentThis.get.associatedList != currentThat.get.associatedList) {
          return false
        }
        currentThis = currentThis.get.next
        currentThat = currentThat.get.next
      }
      true
    case _ => false
  }
  
  override def hashCode(): Int = {
    var result = 1
    var current = root
    
    while (current.isDefined) {
      val headerHash = if (current.get.header != null) current.get.header.hashCode else 0
      val listHash = current.get.associatedList.hashCode
      
      result = 31 * result + headerHash
      result = 31 * result + listHash
      
      current = current.get.next
    }
    result
  }
}

object HeadersList {
  
  //башку в узел завязал(пара заголовок + список, надо для линковки)
  private case class HeaderNode[T](
    var header: T,
    var associatedList: ListBuffer[T],
    var next: Option[HeaderNode[T]]
  )
  
  case class SerializableNode[T](header: T, associatedList: List[T])
  
  def loadFromFile[T <: Comparable[T]](file: File, clazz: Class[T]): HeadersList[T] = {
    import CustomTypes.{CustomDateAdapter, Point2DAdapter}
    
    val gsonBuilder = new GsonBuilder()
      .registerTypeAdapter(classOf[CustomTypes.CustomDate], CustomDateAdapter)
      .registerTypeAdapter(classOf[CustomTypes.Point2D], Point2DAdapter)
    
    val gson = gsonBuilder.create()
    val resultList = new HeadersList[T]()
    val reader = new FileReader(file)
    
    try {
      // Читаем как список Map'ов
      val listType = TypeToken.getParameterized(
        classOf[java.util.List[_]],
        classOf[java.util.Map[_, _]]
      ).getType
      
      val loadedData: java.util.List[java.util.Map[String, Any]] = 
        gson.fromJson(reader, listType)
      
      if (loadedData != null) {
        loadedData.asScala.foreach { nodeMap =>
          // Парсим header
          val header = gson.fromJson(gson.toJson(nodeMap.get("header")), clazz)
          
          // Парсим associatedList
          val listData = nodeMap.get("associatedList").asInstanceOf[java.util.List[_]]
          val parsedList = listData.asScala.map { elem =>
            gson.fromJson(gson.toJson(elem), clazz)
          }.toList
          
          resultList.add(header, parsedList)
        }
      }
    } finally {
      reader.close()
    }
    resultList
  }
  
  def loadFromBinaryFile[T <: Comparable[T]](file: File, clazz: Class[T]): HeadersList[T] = {
    val resultList = new HeadersList[T]()
    val dis = new DataInputStream(new FileInputStream(file))
    
    try {
      val size = dis.readInt()
      
      for (_ <- 0 until size) {
        val header = resultList.readComparable(dis, clazz)
        val listSize = dis.readInt()
        val items = (0 until listSize).map { _ =>
          resultList.readComparable(dis, clazz)
        }.toList
        
        resultList.add(header, items)
      }
    } finally {
      dis.close()
    }
    
    resultList
  }
}
