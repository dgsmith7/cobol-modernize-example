import React, { useState } from "react";

export default function TransferForm({ fromAccount, onSuccess }) {
  const [toAccount, setToAccount] = useState("");
  const [amount, setAmount] = useState("");
  const [message, setMessage] = useState("");
  const [messageType, setMessageType] = useState("info"); // "success" or "danger"

  const validate = () => {
    if (!/^\d{10}$/.test(toAccount)) {
      setMessage("To Account Number must be exactly 10 digits.");
      setMessageType("danger");
      return false;
    }
    if (toAccount === fromAccount) {
      setMessage("Cannot transfer to the same account.");
      setMessageType("danger");
      return false;
    }
    if (isNaN(amount) || Number(amount) <= 0) {
      setMessage("Amount must be a positive number.");
      setMessageType("danger");
      return false;
    }
    return true;
  };

  const handleSubmit = async (e) => {
    e.preventDefault();
    setMessage("");
    if (!validate()) return;
    const res = await fetch("http://localhost:3001/api/transfers", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        fromAccount,
        toAccount,
        amount: parseFloat(amount),
      }),
    });
    const data = await res.json();
    if (data.success) {
      setMessage("Transfer successful!");
      setMessageType("success");
      setToAccount("");
      setAmount("");
      if (onSuccess) onSuccess();
    } else {
      setMessage(data.error?.message || "Transfer failed.");
      setMessageType("danger");
    }
  };

  return (
    <form
      className="card p-3 mb-4"
      style={{ maxWidth: 500 }}
      onSubmit={handleSubmit}
    >
      <h3 className="mb-3">Transfer Funds</h3>
      <div className="mb-3">
        <label className="form-label">To Account Number</label>
        <input
          className="form-control"
          type="text"
          placeholder="To Account Number"
          value={toAccount}
          onChange={(e) => setToAccount(e.target.value)}
          required
        />
      </div>
      <div className="mb-3">
        <label className="form-label">Amount</label>
        <input
          className="form-control"
          type="number"
          step="0.01"
          min="0"
          placeholder="Amount"
          value={amount}
          onChange={(e) => setAmount(e.target.value)}
          required
        />
      </div>
      <button className="btn btn-warning" type="submit">
        Transfer
      </button>
      {message && (
        <div
          className={`mt-3 alert alert-${messageType} text-center fw-bold`}
          role="alert"
        >
          {message}
        </div>
      )}
    </form>
  );
}
