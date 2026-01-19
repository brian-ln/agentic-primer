
import { loadNetworkInfo } from "../daemon/config.ts";
import type { TaskProperties, NodeProperties, CreateTaskOptions } from "./types.ts";

export interface DaemonResponse<T> {
  success: boolean;
  data?: T;
  error?: string;
}

export class DaemonClient {
  private baseUrl: string = "";

  constructor(baseUrl?: string) {
    if (baseUrl) {
      this.baseUrl = baseUrl;
    }
  }

  private async getBaseUrl(): Promise<string> {
    if (this.baseUrl) return this.baseUrl;

    const info = loadNetworkInfo();
    if (!info) {
      // Fallback to default port if no info file exists
      return "http://127.0.0.1:3000";
    }

    if (info.type === "socket") {
      throw new Error("Unix socket not supported by DaemonClient yet");
    }

    this.baseUrl = `http://127.0.0.1:${info.value}`;
    return this.baseUrl;
  }

  private async request<T>(path: string, options: RequestInit = {}): Promise<T> {
    const baseUrl = await this.getBaseUrl();
    const url = `${baseUrl}${path}`;

    const response = await fetch(url, {
      ...options,
      headers: {
        "Content-Type": "application/json",
        ...options.headers,
      },
    });

    if (!response.ok) {
      const errorText = await response.text();
      try {
        const errorJson = JSON.parse(errorText);
        throw new Error(errorJson.error || `HTTP error ${response.status}`);
      } catch {
        throw new Error(`HTTP error ${response.status}: ${errorText}`);
      }
    }

    return await response.json();
  }

  async getHealth(): Promise<any> {
    return this.request("/api/health");
  }

  async getStats(): Promise<any> {
    return this.request("/api/stats");
  }

  async listTasks(filters: { status?: string; label?: string; priority?: number } = {}): Promise<any[]> {
    const params = new URLSearchParams();
    if (filters.status) params.append("status", filters.status);
    if (filters.label) params.append("label", filters.label);
    if (filters.priority !== undefined) params.append("priority", filters.priority.toString());

    const queryString = params.toString() ? `?${params.toString()}` : "";
    return this.request(`/api/tasks${queryString}`);
  }

  async getTask(id: string): Promise<any> {
    return this.request(`/api/tasks/${id}`);
  }

  async createTask(options: any): Promise<any> {
    return this.request("/api/tasks", {
      method: "POST",
      body: JSON.stringify(options),
    });
  }

  async updateTask(id: string, action: "start" | "complete" | "block", data: any = {}): Promise<any> {
    return this.request(`/api/tasks/${id}`, {
      method: "PUT",
      body: JSON.stringify({ action, ...data }),
    });
  }

  async deleteTask(id: string): Promise<any> {
    return this.request(`/api/tasks/${id}`, {
      method: "DELETE",
    });
  }

  async listReviews(status?: string): Promise<any[]> {
    const path = status ? `/api/reviews?status=${status}` : "/api/reviews";
    return this.request(path);
  }

  async approveReview(id: string, comment: string = ""): Promise<any> {
    return this.request(`/api/reviews/${id}/approve`, {
      method: "POST",
      body: JSON.stringify({ comment }),
    });
  }

  async rejectReview(id: string, comment: string): Promise<any> {
    return this.request(`/api/reviews/${id}/reject`, {
      method: "POST",
      body: JSON.stringify({ comment }),
    });
  }

  async startReview(id: string): Promise<any> {
    return this.request(`/api/reviews/${id}/start`, {
      method: "POST",
      body: JSON.stringify({}),
    });
  }

  async listAgents(): Promise<any[]> {
    return this.request("/api/agents");
  }

  async getTimeline(options: { type?: string; limit?: number } = {}): Promise<any[]> {
    const params = new URLSearchParams();
    if (options.type) params.append("type", options.type);
    if (options.limit) params.append("limit", options.limit.toString());

    const queryString = params.toString() ? `?${params.toString()}` : "";
    return this.request(`/api/timeline${queryString}`);
  }

  async evalTask(id: string): Promise<any> {
    return this.request(`/api/tasks/${id}/eval`, {
      method: "POST",
    });
  }

  async getTaskStatus(id: string): Promise<any> {
    return this.request(`/api/tasks/${id}/status`);
  }

  async searchTasks(query: string): Promise<any[]> {
    return this.request(`/api/tasks/search?q=${encodeURIComponent(query)}`);
  }

  async addEdge(fromId: string, toId: string, type: string, properties: any = {}): Promise<any> {
    return this.request("/api/edges", {
      method: "POST",
      body: JSON.stringify({ fromId, toId, type, properties }),
    });
  }

  async deleteEdge(edgeId: string): Promise<any> {
    return this.request(`/api/edges/${edgeId}`, {
      method: "DELETE",
    });
  }

  async queryCozoReady(): Promise<any[]> {
    return this.request("/api/queries/ready");
  }

  async queryCozoBlocked(): Promise<any[]> {
    return this.request("/api/queries/blocked");
  }

  async queryCozoSearch(keyword: string): Promise<any[]> {
    return this.request(`/api/queries/search?q=${encodeURIComponent(keyword)}`);
  }

  async batchAddTasks(specs: any[]): Promise<{ created: string[]; errors: any[] }> {
    return this.request("/api/batch/tasks", {
      method: "POST",
      body: JSON.stringify({ specs }),
    });
  }

  async batchUpdateTasks(specs: any[]): Promise<{ updated: string[]; errors: any[] }> {
    return this.request("/api/batch/tasks/update", {
      method: "POST",
      body: JSON.stringify({ specs }),
    });
  }

  async listNodes(type?: string): Promise<any[]> {
    const query = type ? `?type=${type}` : "";
    return this.request(`/api/nodes${query}`);
  }

  async getNode(id: string): Promise<any> {
    return this.request(`/api/nodes/${id}`);
  }

  async createNode(data: any): Promise<any> {
    return this.request("/api/nodes", {
      method: "POST",
      body: JSON.stringify(data),
    });
  }

  async deleteNode(id: string): Promise<any> {
    return this.request(`/api/nodes/${id}`, {
      method: "DELETE",
    });
  }

  async listEdges(filters: { from?: string; to?: string; type?: string } = {}): Promise<any[]> {
    const params = new URLSearchParams();
    if (filters.from) params.append("from", filters.from);
    if (filters.to) params.append("to", filters.to);
    if (filters.type) params.append("type", filters.type);

    const queryString = params.toString() ? `?${params.toString()}` : "";
    return this.request(`/api/edges${queryString}`);
  }

  async getGraphDump(): Promise<any> {
    return this.request("/api/graph/dump");
  }
}
