//===- Hello.cpp - Example code from "Writing an LLVM Pass" ---------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements two versions of the LLVM "Hello World" pass described
// in docs/WritingAnLLVMPass.html
//
//===----------------------------------------------------------------------===//
#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include <set>
#include <iostream>

using namespace llvm;

static cl::opt<unsigned long long> addr("offsetaddr", cl::desc("Specify offset of shadow memory"), cl::value_desc("offsetaddr"));

namespace {
  struct TwinAccess : public ModulePass {
  bool runOnModule(Module &M);
    static char ID; 
    TwinAccess() : ModulePass(ID) {}

    virtual const char *getPassName() const {
      return "Twinaccess";
    }
  };
}

char TwinAccess::ID = 0;
static RegisterPass<TwinAccess> X("twinaccess", "twin access", false, false);
/*
int setStructMetadata(llvm::Value* I) {
    //errs() << *structptr << "\n";
    llvm::PointerType* mypt = dyn_cast<llvm::PointerType>(I->getType());
    llvm::StructType* mystruct = dyn_cast<llvm::StructType>(mypt->getElementType());
    for (StructType::element_iterator mybegin = mystruct->element_begin(), myend = mystruct->element_end();mybegin != myend; ++mybegin) {
        if ((*mybegin)->getMetadata("tyann")) {
            //errs() << (*mybegin)->getTypeID() << "\n";
        }
    }
    return 0;
}
*/
bool TwinAccess::runOnModule(Module &M) {
    LLVMContext &Context = M.getContext();
    Type *Int8Ty = Type::getInt8Ty(Context);
    Type *Int16Ty = Type::getInt16Ty(Context);
    Type *Int32Ty = Type::getInt32Ty(Context);
    Type *Int64Ty = Type::getInt64Ty(Context);
    //Type *Int64PtrTy = Type::getInt64PtrTy(Context);
    Value *OffsetAddr = ConstantInt::get(Int64Ty, addr);
    
    std::vector<Instruction*> insertpos;
    for (Module::iterator F = M.begin(), E = M.end(); F != E; ++F) {
        if (F->isDeclaration()) continue;
        for (Function::iterator BB = F->begin(), E = F->end(); BB != E; ++BB){
            for(BasicBlock::iterator I = BB->begin(),IE = BB->end();I!= IE;++I){
                //if(I->getOpcode()==27 || I->getOpcode()==28){
                if(I->getOpcode()==Instruction::Load) {
                   if (I->getMetadata("tyann")) {
                       /*errs() << *I << "\n";*/
                       insertpos.push_back(I);
                       //BB->splitBasicBlock(I, "");
                       //return false;
                    }
                }//endif
              /*  if (I->getOpcode()==Instruction::Alloca) {
                    errs() << "struct: " << I->getType()->getTypeID() << "\n";
                    llvm::PointerType* mypt = dyn_cast<llvm::PointerType>(I->getType());
                    if (mypt->getElementType()->isStructTy()) {
                        setStructMetadata(I);
                    }
                    errs() << "structid   " << llvm::Type::StructTyID << "\n";
                    errs() << "integerid  " << llvm::Type::IntegerTyID << "\n";
                    errs() << "pointerid  " << llvm::Type::PointerTyID << "\n";
                }*/
            }//outBasic
        }//outfuncton
    }//outmodule 
    for (int i = 0; i < insertpos.size(); i++) {
        /*errs() << *insertpos[i] << "\n\n";*/
        Instruction *t1 = insertpos[i]->clone();
        insertpos[i]->getParent()->getInstList().insert(insertpos[i], t1);
        Value* oprend = t1;
        //if (insertpos[i]->getOpcode() == Instruction::Load) oprend = t1;
        //if (insertpos[i]->getOpcode() == Instruction::Store) oprend = t1->getOperand(0);
        //if (insertpos[i]->getType()->isIntegerTy()) {
        if (oprend->getType()->isIntegerTy()) {
            Value *ErrorValue = NULL;
            //errs() << insertpos[i]->getType()->getIntegerBitWidth() << "\n\n";
            //switch (insertpos[i]->getType()->getIntegerBitWidth()) {
            switch (oprend->getType()->getIntegerBitWidth()) {
                case 8: ErrorValue = ConstantInt::get(Int8Ty, 0x5a);
                        break;
                case 16: ErrorValue = ConstantInt::get(Int16Ty, 0x5a5a);
                         break;
                case 32: ErrorValue = ConstantInt::get(Int32Ty, 0x5a5a5a5a);
                         break;
                case 64: ErrorValue = ConstantInt::get(Int64Ty, 0x5a5a5a5a5a5a5a5a);
                         break;
                default: errs() << "something error in integer type!" << "\n";
                         return false;
            }
            Value* memaddr = insertpos[i]->getOperand(insertpos[i]->getNumOperands()-1);
            Type* memtype = memaddr->getType();
            memaddr = new PtrToIntInst(memaddr, Int64Ty, "addrtoint", insertpos[i]);
            Value* newaddr = BinaryOperator::Create(Instruction::Add, memaddr, OffsetAddr, "newaddr", insertpos[i]);
            newaddr = new IntToPtrInst(newaddr, memtype, "inttoaddr", insertpos[i]);
            Instruction *newt1 = insertpos[i]->clone();
            newt1->setOperand(newt1->getNumOperands()-1, newaddr);
            insertpos[i]->getParent()->getInstList().insert(insertpos[i], newt1);
            ICmpInst* myicmp = new ICmpInst(insertpos[i], llvm::CmpInst::ICMP_EQ, oprend, ErrorValue, "errorcmp");
            BasicBlock* oldblock = insertpos[i]->getParent();
            BasicBlock* if_then = BasicBlock::Create(Context, "twinac_if.then", insertpos[i]->getParent()->getParent(), 0);
            BasicBlock* if_else = BasicBlock::Create(Context, "twinac_if.else", insertpos[i]->getParent()->getParent(), 0);
            BasicBlock* if_end = insertpos[i]->getParent()->splitBasicBlock(insertpos[i], "twinac_if.end");
            oldblock->getTerminator()->eraseFromParent();
            BranchInst::Create(if_then, if_else, myicmp, oldblock);
            llvm::BranchInst::Create(if_end, if_then);
            llvm::BranchInst::Create(if_end, if_else);
            PHINode* finalvalue = PHINode::Create(oprend->getType(), 2, "finavalue", insertpos[i]);
            finalvalue->addIncoming(t1, if_else);
            finalvalue->addIncoming(newt1, if_then);
            insertpos[i]->replaceAllUsesWith(finalvalue);
            insertpos[i]->eraseFromParent();

            /*ICmpInst* myicmp = new ICmpInst(insertpos[i], llvm::CmpInst::ICMP_EQ, oprend, ErrorValue, "errorcmp");
            BasicBlock* oldblock = insertpos[i]->getParent();
            BasicBlock* if_end = insertpos[i]->getParent()->splitBasicBlock(insertpos[i], "twinac_if.end");
            BasicBlock* if_then = BasicBlock::Create(Context, "twinac_if.then", insertpos[i]->getParent()->getParent(), 0);
            Value* memaddr = insertpos[i]->getOperand(insertpos[i]->getNumOperands()-1);
            Type* memtype = memaddr->getType();
            memaddr = new PtrToIntInst(memaddr, Int64Ty, "addrtoint", if_then);
            Value* newaddr = BinaryOperator::Create(Instruction::Add, memaddr, OffsetAddr, "newaddr", if_then);
            newaddr = new IntToPtrInst(newaddr, memtype, "inttoaddr", if_then);
            Instruction *newt1 = insertpos[i]->clone();
            newt1->setOperand(newt1->getNumOperands()-1, newaddr);
            if_then->getInstList().push_back(newt1);
            llvm::BranchInst::Create(if_end, if_then);
            oldblock->getTerminator()->eraseFromParent();
            BranchInst::Create(if_then, if_end, myicmp, oldblock);
            PHINode* finalvalue = PHINode::Create(oprend->getType(), 2, "finavalue", insertpos[i]);
            finalvalue->addIncoming(t1, oldblock);
            finalvalue->addIncoming(newt1, if_then);
            //ReplaceInstWithValue(if_end->getInstList(), insertpos[i], finalvalue);
            insertpos[i]->replaceAllUsesWith(finalvalue);
            insertpos[i]->eraseFromParent();*/
        } else if (oprend->getType()->isFloatingPointTy()) {
            Value* ErrorValue = NULL;
            switch (oprend->getType()->getTypeID()) {
                case Type::HalfTyID:
                    /*double tmpdouble;
                    *((int64_t*)(&tmpdouble)) = 0x5a5a5a5a5a5a5a5a;
                    ErrorValue = ConstantFP::get(oprend->getType(), tmpdouble);
                    break;*/
                case Type::FloatTyID:
                    float tmpfloat;
                    *((int32_t*)(&tmpfloat)) = 0x5a5a5a5a;
                    ErrorValue = ConstantFP::get(Context, APFloat(tmpfloat));
                    break;
                case Type::DoubleTyID:
                    double tmpdouble;
                    *((int64_t*)(&tmpdouble)) = 0x5a5a5a5a5a5a5a5a;
                    ErrorValue = ConstantFP::get(Context, APFloat(tmpdouble));
                    //ErrorValue = ConstantFP::get(oprend->getType(), tmpdouble);
                    break;
                default:
                    errs() << "something error in integer type!" << "\n";
                    return false;
            }
            
            Value* memaddr = insertpos[i]->getOperand(insertpos[i]->getNumOperands()-1);
            Type* memtype = memaddr->getType();
            memaddr = new PtrToIntInst(memaddr, Int64Ty, "addrtoint", insertpos[i]);
            Value* newaddr = BinaryOperator::Create(Instruction::Add, memaddr, OffsetAddr, "newaddr", insertpos[i]);
            newaddr = new IntToPtrInst(newaddr, memtype, "inttoaddr", insertpos[i]);
            Instruction *newt1 = insertpos[i]->clone();
            newt1->setOperand(newt1->getNumOperands()-1, newaddr);
            insertpos[i]->getParent()->getInstList().insert(insertpos[i], newt1);
            FCmpInst* myfcmp = new FCmpInst(insertpos[i], llvm::CmpInst::FCMP_OEQ, oprend, ErrorValue, "errorcmp");
            BasicBlock* oldblock = insertpos[i]->getParent();
            BasicBlock* if_then = BasicBlock::Create(Context, "twinac_if.then", insertpos[i]->getParent()->getParent(), 0);
            BasicBlock* if_else = BasicBlock::Create(Context, "twinac_if.else", insertpos[i]->getParent()->getParent(), 0);
            BasicBlock* if_end = insertpos[i]->getParent()->splitBasicBlock(insertpos[i], "twinac_if.end");
            oldblock->getTerminator()->eraseFromParent();
            BranchInst::Create(if_then, if_else, myfcmp, oldblock);
            llvm::BranchInst::Create(if_end, if_then);
            llvm::BranchInst::Create(if_end, if_else);
            PHINode* finalvalue = PHINode::Create(oprend->getType(), 2, "finavalue", insertpos[i]);
            finalvalue->addIncoming(t1, if_else);
            finalvalue->addIncoming(newt1, if_then);
            insertpos[i]->replaceAllUsesWith(finalvalue);
            insertpos[i]->eraseFromParent();

            /*FCmpInst* myfcmp = new FCmpInst(insertpos[i], llvm::CmpInst::FCMP_OEQ, oprend, ErrorValue, "errorcmp");
            BasicBlock* oldblock = insertpos[i]->getParent();
            BasicBlock* if_end = insertpos[i]->getParent()->splitBasicBlock(insertpos[i], "twinac_if.end");
            BasicBlock* if_then = BasicBlock::Create(Context, "twinac_if.then", insertpos[i]->getParent()->getParent(), 0);
            Value* memaddr = insertpos[i]->getOperand(insertpos[i]->getNumOperands()-1);
            Type* memtype = memaddr->getType();
            memaddr = new PtrToIntInst(memaddr, Int64Ty, "addrtoint", if_then);
            Value* newaddr = BinaryOperator::Create(Instruction::Add, memaddr, OffsetAddr, "newaddr", if_then);
            newaddr = new IntToPtrInst(newaddr, memtype, "inttoaddr", if_then);
            Instruction *newt1 = insertpos[i]->clone();
            newt1->setOperand(newt1->getNumOperands()-1, newaddr);
            if_then->getInstList().push_back(newt1);
            llvm::BranchInst::Create(if_end, if_then);
            oldblock->getTerminator()->eraseFromParent();
            BranchInst::Create(if_then, if_end, myfcmp, oldblock);
            PHINode* finalvalue = PHINode::Create(oprend->getType(), 2, "finavalue", insertpos[i]);
            finalvalue->addIncoming(t1, oldblock);
            finalvalue->addIncoming(newt1, if_then);
            //ReplaceInstWithValue(if_end->getInstList(), insertpos[i], finalvalue);
            insertpos[i]->replaceAllUsesWith(finalvalue);
            insertpos[i]->eraseFromParent();*/

        }
        /*Value* memaddr = insertpos[i]->getOperand(insertpos[i]->getNumOperands()-1);
        Type* memtype = memaddr->getType();
        memaddr = new PtrToIntInst(memaddr, Int64Ty, "addrtoint", insertpos[i]);
        Value* newaddr = BinaryOperator::Create(Instruction::Add, memaddr, OffsetAddr, "newaddr", insertpos[i]);
        newaddr = new IntToPtrInst(newaddr, memtype, "inttoaddr", insertpos[i]);
        Instruction *newt1 = insertpos[i]->clone();
        newt1->setOperand(newt1->getNumOperands()-1, newaddr);
        insertpos[i]->getParent()->getInstList().insert(insertpos[i], newt1);*/
        //insertpos[i]->getParent()->splitBasicBlock(insertpos[i], "mytest");
    }
    return false;
}//endrunOnModule
