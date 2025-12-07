package HeadersList

object HeadersListFactory {
  
  private def copy[T <: Comparable[T]](original: HeadersList[T]): HeadersList[T] = {
    val newInstance = new HeadersList[T]()
    
    original.foreach { nodeData =>
      newInstance.add(nodeData.header, nodeData.associatedList)
    }
    
    newInstance
  }
  
  def add[T <: Comparable[T]](source: HeadersList[T], header: T, list: List[T]): HeadersList[T] = {
    val newInstance = copy(source)
    newInstance.add(header, list)
    newInstance
  }
  
  def add[T <: Comparable[T]](source: HeadersList[T], index: Int, header: T, list: List[T]): HeadersList[T] = {
    val newInstance = copy(source)
    newInstance.add(index, header, list)
    newInstance
  }
  
  def addSorted[T <: Comparable[T]](source: HeadersList[T], header: T, list: List[T]): HeadersList[T] = {
    val newInstance = copy(source)
    newInstance.addSorted(header, list)
    newInstance
  }
  
  def remove[T <: Comparable[T]](source: HeadersList[T], index: Int): HeadersList[T] = {
    val newInstance = copy(source)
    newInstance.remove(index)
    newInstance
  }
  
  def sort[T <: Comparable[T]](source: HeadersList[T]): HeadersList[T] = {
    val newInstance = copy(source)
    newInstance.sort()
    newInstance
  }
  
  def sortFunctional[T <: Comparable[T]](source: HeadersList[T]): HeadersList[T] = {
    val newInstance = copy(source)
    newInstance.sortFunctional()
    newInstance
  }
  
  def balance[T <: Comparable[T]](source: HeadersList[T]): HeadersList[T] = {
    val newInstance = copy(source)
    newInstance.balance()
    newInstance
  }
}
