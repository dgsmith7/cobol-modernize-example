import { useState, useCallback } from "react";

export default function useApi() {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState("");
  const [data, setData] = useState(null);

  const request = useCallback(async (url, options = {}) => {
    setLoading(true);
    setError("");
    setData(null);
    try {
      const res = await fetch(url, options);
      const json = await res.json();
      if (!json.success) throw new Error(json.error?.message || "API error");
      setData(json.data);
      return json.data;
    } catch (err) {
      setError(err.message || "Network error");
      throw err;
    } finally {
      setLoading(false);
    }
  }, []);

  return { loading, error, data, request };
}
