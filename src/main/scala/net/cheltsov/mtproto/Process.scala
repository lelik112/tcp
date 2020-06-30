package net.cheltsov.mtproto

trait State

case object WaitForReqPq extends State

case object WaitForReqDHParams extends State

case object Done extends State

class Process {

}
